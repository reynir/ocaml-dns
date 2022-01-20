(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Dns

module N = Domain_name.Set

let pp_err ppf = function
  | `Cache_miss -> Fmt.string ppf "cache miss"
  | `Cache_drop -> Fmt.string ppf "cache drop"

let pp_question ppf (name, typ) =
  Fmt.pf ppf "%a (%a)" Domain_name.pp name Packet.Question.pp_qtype typ

let find_nearest_ns rng ip_proto ts t name =
  let pick = function
    | [] -> None
    | [ x ] -> Some x
    | xs -> Some (List.nth xs (Randomconv.int ~bound:(List.length xs) rng))
  in
  let find_ns name = match snd (Dns_cache.get t ts name Ns) with
    | Ok `Entry (_, names) -> Domain_name.Host_set.elements names
    | _ -> []
  and find_address name =
    let ip4s =
      Result.fold
        ~ok:(function
            | `Entry (_, ips) ->
              List.map (fun ip -> Ipaddr.V4 ip) (Ipaddr.V4.Set.elements ips)
            | _ -> [])
        ~error:(fun _ -> [])
        (snd (Dns_cache.get t ts name A))
    and ip6s =
      Result.fold
        ~ok:(function
            | `Entry (_, ips) ->
              List.map (fun ip -> Ipaddr.V6 ip) (Ipaddr.V6.Set.elements ips)
            | _ -> [])
        ~error:(fun _ -> [])
        (snd (Dns_cache.get t ts name Aaaa))
    in
    match ip_proto with
    | `Both -> ip4s @ ip6s
    | `Ipv4_only -> ip4s
    | `Ipv6_only -> ip6s
  in
  let or_root f nam =
    if Domain_name.(equal root nam) then
      match pick (Dns_resolver_root.ips ip_proto) with
      | None -> assert false
      | Some ip -> `HaveIP (Domain_name.root, ip)
    else
      f (Domain_name.drop_label_exn nam)
  in
  let rec go nam =
    match pick (find_ns nam) with
    | None -> or_root go nam
    | Some ns ->
      let host = Domain_name.raw ns in
      match pick (find_address host) with
      | None ->
        if Domain_name.is_subdomain ~subdomain:ns ~domain:nam then
          (* we actually need glue *)
          or_root go nam
        else
          `NeedAddress host
      | Some ip -> `HaveIP (nam, ip)
  in
  go name

let resolve t ~rng ip_proto ts name typ =
  (* the standard recursive algorithm *)
  let addresses = match ip_proto with
    | `Both -> [`K (Rr_map.K A); `K (Rr_map.K Aaaa)]
    | `Ipv4_only -> [`K (Rr_map.K A)]
    | `Ipv6_only -> [`K (Rr_map.K Aaaa)]
  in
  (* with DNSSec:
     - input is qname and qtyp
     - (a) we have (validated) NS record (+DNSKEY) for zone -> move along
     - (b) we miss a NS entry -> drop label and find one
     ---> we also want to collect DS and DNSKEY entries (or non-existence of DS)
     ---> we get DS by dnssec ok in EDNS
     ---> we may have unsigned NS (+ glue), and need to ask the NS for NS (+dnssec)
     ---> we may have unsigned glue, and need to go down for signed A/AAAA
  *)
  let rec go t types name =
    Logs.debug (fun m -> m "go %a" Domain_name.pp name) ;
    match find_nearest_ns rng ip_proto ts t (Domain_name.raw name) with
    | `NeedAddress ns -> go t addresses ns
    | `HaveIP (zone, ip) -> zone, name, types, ip, t
  in
  go t [typ] name

let to_map (name, soa) = Name_rr_map.singleton name Soa soa

let follow_cname t ts typ ~name ttl ~alias =
  let rec follow t acc name =
    let t, r = Dns_cache.get_or_cname t ts name typ in
    match r with
    | Error _ ->
      Logs.debug (fun m -> m "follow_cname: cache miss, need to query %a"
                     Domain_name.pp name);
      `Query name, t
    | Ok `Alias (_, alias) ->
      let acc' = Domain_name.Map.add name (Rr_map.singleton Cname (ttl, alias)) acc in
      if Domain_name.Map.mem alias acc then begin
        Logs.warn (fun m -> m "follow_cname: cycle detected") ;
        `Out (Rcode.NoError, acc', Name_rr_map.empty), t
      end else begin
        Logs.debug (fun m -> m "follow_cname: alias to %a, follow again"
                       Domain_name.pp alias);
        follow t acc' alias
      end
    | Ok `Entry v ->
      let acc' = Domain_name.Map.add name Rr_map.(singleton typ v) acc in
      Logs.debug (fun m -> m "follow_cname: entry found, returning");
      `Out (Rcode.NoError, acc', Name_rr_map.empty), t
    | Ok `No_domain res ->
      Logs.debug (fun m -> m "follow_cname: nodom");
      `Out (Rcode.NXDomain, acc, to_map res), t
    | Ok `No_data res ->
      Logs.debug (fun m -> m "follow_cname: nodata");
      `Out (Rcode.NoError, acc, to_map res), t
    | Ok `Serv_fail res ->
      Logs.debug (fun m -> m "follow_cname: servfail") ;
      `Out (Rcode.ServFail, acc, to_map res), t
  in
  let initial = Name_rr_map.singleton name Cname (ttl, alias) in
  follow t initial alias

let answer t ts name typ =
  let packet _t _add rcode answer authority =
    (* TODO why was this RA + RD in here? should not be RD for recursive algorithm
       TODO should it be authoritative for recursive algorithm? *)
    let data = (answer, authority) in
    let flags = Packet.Flags.singleton `Recursion_desired
    (* XXX: we should look for a fixpoint here ;) *)
    (*    and additional, t = if add then additionals t ts answer else [], t *)
    and data = match rcode with
      | Rcode.NoError -> `Answer data
      | x ->
        let data = if Packet.Answer.is_empty data then None else Some data in
        `Rcode_error (x, Opcode.Query, data)
    in
    flags, data
  in
  match typ with
  | `Any ->
    let t, r = Dns_cache.get_any t ts name in
    begin match r with
      | Error e ->
        Logs.warn (fun m -> m "error %a while looking up %a, query"
                      pp_err e pp_question (name, typ));
        `Query name, t
      | Ok `No_domain res ->
        Logs.debug (fun m -> m "no domain while looking up %a, query" pp_question (name, typ));
        `Packet (packet t false Rcode.NXDomain Domain_name.Map.empty (to_map res)), t
      | Ok `Entries rr_map ->
        Logs.debug (fun m -> m "entries while looking up %a" pp_question (name, typ));
        let data = Domain_name.Map.singleton name rr_map in
        `Packet (packet t true Rcode.NoError data Domain_name.Map.empty), t
    end
  | `K (Rr_map.K ty) ->
    let t, r = Dns_cache.get_or_cname t ts name ty in
    match r with
    | Error e ->
      Logs.warn (fun m -> m "error %a while looking up %a, query"
                    pp_err e pp_question (name, typ));
      `Query name, t
    | Ok `No_domain res ->
      Logs.debug (fun m -> m "no domain while looking up %a, query" pp_question (name, typ));
      `Packet (packet t false Rcode.NXDomain Domain_name.Map.empty (to_map res)), t
    | Ok `No_data res ->
      Logs.debug (fun m -> m "no data while looking up %a" pp_question (name, typ));
      `Packet (packet t false Rcode.NoError Domain_name.Map.empty (to_map res)), t
    | Ok `Serv_fail res ->
      Logs.debug (fun m -> m "serv fail while looking up %a" pp_question (name, typ));
      `Packet (packet t false Rcode.ServFail Domain_name.Map.empty (to_map res)), t
    | Ok `Alias (ttl, alias) ->
      begin
        Logs.debug (fun m -> m "alias while looking up %a" pp_question (name, typ));
        match ty with
        | Cname ->
          let data = Name_rr_map.singleton name Cname (ttl, alias) in
          `Packet (packet t false Rcode.NoError data Domain_name.Map.empty), t
        | ty ->
          match follow_cname t ts ty ~name ttl ~alias with
          | `Out (rcode, an, au), t -> `Packet (packet t true rcode an au), t
          | `Query n, t -> `Query n, t
      end
    | Ok `Entry v ->
      Logs.debug (fun m -> m "entry while looking up %a" pp_question (name, typ));
      let data = Name_rr_map.singleton name ty v in
      `Packet (packet t true Rcode.NoError data Domain_name.Map.empty), t

let handle_query t ~rng ip_proto ts (qname, qtype) =
  match answer t ts qname qtype with
  | `Packet (flags, data), t -> `Reply (flags, data), t
  | `Query name, t ->
    let zone, name', types, ip, t = resolve t ~rng ip_proto ts name qtype in
    Logs.debug (fun m -> m "resolve returned zone %a query %a (%a), ip %a"
                   Domain_name.pp zone Domain_name.pp name'
                   Fmt.(list ~sep:(any ", ") Packet.Question.pp_qtype) types
                   Ipaddr.pp ip);
    `Query (zone, (name', types), ip), t
