
(* This file is free software. See file "license" for more details. *)

(** {1 Abstract Syntax Tree} *)

type name = string
type flag = string
type test = string

type expr =
  | E_flag of flag  (* flag(doc) *)
  | E_test of test * string (* os_type(Win32) *)
  | E_true
  | E_false
  | E_and of expr * expr
  | E_or of expr * expr
  | E_not of expr

let e_flag f = E_flag f
let e_test t s = E_test (t,s)
let e_true = E_true
let e_false = E_false
let e_and a b = E_and (a,b)
let e_or a b = E_or (a,b)
let e_not a = E_not a

type field_op =
  | F_set of string list (* foo: bar *)
  | F_add of string list (* foo+: bar *)
  | F_eval of expr (* foo$: bar *)

let f_set s = F_set s
let f_add s = F_add s
let f_eval e = F_eval e

type stmt =
  | S_field of name * field_op
  | S_if of expr * stmt * stmt

let s_field n o = S_field (n,o)
let s_if a b c = S_if (a,b,c)

type toplevel_decl =
  | Library
  | Object
  | Executable
  | Source_repository
  | Test
  | Document

type top_stmt =
  | TS_decl of toplevel_decl * name * stmt list
  | TS_stmt of stmt

let ts_decl d n s = TS_decl (d,n,s)
let ts_stmt s = TS_stmt s

let rec pp_expr out = function
  | E_flag s -> Format.fprintf out "flag(%s)" s
  | E_test (n,s) -> Format.fprintf out "%s(%s)" n s
  | E_true -> Format.fprintf out "true"
  | E_false -> Format.fprintf out "false"
  | E_and (a,b) ->
    Format.fprintf out "(@[@[%a@]@ && @[%a@]@])" pp_expr a pp_expr b
  | E_or (a,b) ->
    Format.fprintf out "(@[@[%a@]@ || @[%a@]@])" pp_expr a pp_expr b
  | E_not e -> Format.fprintf out "!@[%a@]" pp_expr e

let rec pp_list pp out = function
  | [] -> ()
  | [x] -> pp out x
  | x :: tail ->
    Format.fprintf out "%a@,%a" pp x (pp_list pp) tail

let rec pp_stmt out s =
  match s with
    | S_field (n, F_set l) ->
      Format.fprintf out "@[%s: @[<v>%a@]@]" n
        (pp_list Format.pp_print_string) l
    | S_field (n, F_add l) ->
      Format.fprintf out "@[%s+: @[<v>%a@]@]" n
        (pp_list Format.pp_print_string) l
    | S_field (n, F_eval e) ->
      Format.fprintf out "@[%s$: @[<hv>%a@]@]" n pp_expr e
    | S_if (e, a, b) ->
      Format.fprintf out "@[<v>if @[<h>%a@]@ @[<2>  %a@]@ else@ @[<2>  %a@]@]"
        pp_expr e pp_stmt a pp_stmt b

let pp_top_stmt out st =
  let pp_decl out d =
    Format.fprintf out "%s"
      (match d with
        | Library -> "Library"
        | Object -> "Object"
        | Executable -> "Executable"
        | Source_repository -> "Source_repository"
        | Test -> "Test"
        | Document -> "Document"
      )
  in match st with
    | TS_stmt s -> pp_stmt out s
    | TS_decl (d,n,l) ->
      Format.fprintf out "@[<v2>%a %s:@ %a@]@,"
        pp_decl d n (pp_list pp_stmt) l
