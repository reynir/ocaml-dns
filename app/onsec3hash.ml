let jump () salt iterations domain =
  Logs.debug (fun m -> m "Hashing with salt %a using %d iterations on domain %a"
                 (Fmt.of_to_string Base32.encode) salt
                 iterations
                 Domain_name.pp domain);
  let domain = Domain_name.canonical domain in
  let hash = Dnssec.nsec3_hash (Cstruct.of_string salt) iterations domain in
  print_endline (Base32.encode (Cstruct.to_string hash))

open Cmdliner

let to_presult = function
  | Ok a -> `Ok a
  | Error s -> `Error s

let parse_base32 : string Arg.conv =
  (fun s ->
     match Base32.decode ~unpadded:true s with
     | Ok s -> `Ok s
     | Error `Msg e -> `Error e),
  Fmt.(of_to_string (fun s -> Base32.encode s))


let arg_salt =
  let doc = "Salt to use for NSEC3 hash" in
  Arg.(required & pos 0 (some parse_base32) None & info [] ~docv:"SALT" ~doc)

let arg_iterations =
  let doc = "Number of hashing iterations" in
  Arg.(required & pos 1 (some int) None & info [] ~docv:"ITERATIONS" ~doc)

let parse_domain : [ `raw ] Domain_name.t Arg.conv =
  (fun name ->
     Result.map_error
       (function `Msg m -> Fmt.str "Invalid domain: %S: %s" name m)
       (Domain_name.of_string name)
     |> to_presult),
  Domain_name.pp

let arg_domain : [ `raw ] Domain_name.t Term.t =
  let doc = "Domain name to hash" in
  Arg.(required & pos 2 (some parse_domain) None
       & info [] ~docv:"HOST" ~doc)

let cmd =
  Term.(const jump $ Dns_cli.setup_log $ arg_salt $ arg_iterations $ arg_domain),
  Term.info "onsec3hash" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
