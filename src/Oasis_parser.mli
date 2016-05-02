
(* This file is free software. See file "license" for more details. *)

(** {1 The parser} *)

open Oasis_ast

type 'a parser_ = 'a Oasis_parser_co.t

val parse_expr : expr parser_

val parse_stmt : int -> stmt parser_

val parse_top_stmt : top_stmt parser_

val parse : top_stmt list parser_
(** The main parser *)

val parse_file : string -> top_stmt list
(** Parse a file
    @raise Oasis_parser_co.ParseError on failure *)

val parse_string : string -> top_stmt list
(** Parse a string
    @raise Oasis_parser_co.ParseError on failure *)

val split_list : string list -> string list
(** Given a list of blobs (as found in [Oasis_ast.F_set] and [Oasis_ast.F_add]),
    parse them as a list of comma-separated values *)
