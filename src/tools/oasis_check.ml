
(* This file is free software. See file "license" for more details. *)

(** {1 Simple Tool to Check _oasis files} *)

module A = Oasis_ast
module P = Oasis_parser

type conf = {
  debug: bool;
  print: bool;
}

let check_file conf file =
  if conf.debug then Format.printf "check file `%s`@." file;
  try
    let stmts = P.parse_file file in
    if conf.debug then Format.printf "success (%d statements)@." (List.length stmts);
    if conf.print
    then List.iter (Format.printf "%a@." A.pp_top_stmt) stmts;
  with e ->
    let stack = Printexc.get_backtrace() in
    Format.printf "error: %s%s@." (Printexc.to_string e) stack;
    exit 1

let () =
  let l = ref [] in
  let debug = ref false in
  let print = ref false in
  let options =
    Arg.align
      [ "-d", Arg.Set debug, " enable debug mode"
      ; "--print", Arg.Set print, " print parsed statements"
      ]
  in
  Arg.parse
    options
    (fun file -> l := file :: !l)
    "usage: oasis_check [options] file [,file]*";
  let conf = { debug= !debug; print= !print; } in
  List.iter (check_file conf) (List.rev !l)
