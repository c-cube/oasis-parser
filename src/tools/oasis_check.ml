
(* This file is free software. See file "license" for more details. *)

(** {1 Simple Tool to Check _oasis files} *)

module P = Oasis_parser

let check_file debug file =
  if debug then Format.printf "check file `%s`@." file;
  try
    let _stmts = P.parse_file file in
    if debug then Format.printf "success@.";
    (* TODO: with another flag, print back *)
  with e ->
    let stack = Printexc.get_backtrace() in
    Format.printf "error: %s%s" (Printexc.to_string e) stack;
    exit 1

let () =
  let l = ref [] in
  let debug = ref false in
  let options =
    Arg.align
      [ "-d", Arg.Set debug, " enable debug mode"
      ]
  in
  Arg.parse
    options
    (fun file -> l := file :: !l)
    "usage: oasis_check [options] file [,file]*";
  List.iter (check_file !debug) (List.rev !l)
