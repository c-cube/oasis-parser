
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
  | S_if of expr * stmt list * stmt list

let s_field n o = S_field (n,o)
let s_if a b c = S_if (a, b, c)
let s_if_l a ~then_:b ~elif:l ~else_:c =
  let rec aux a b c l : stmt = match l with
    | [] -> s_if a b c
    | (test,case) :: l' -> s_if a b [aux test case c l']
  in
  aux a b c l

type toplevel_decl =
  | Flag
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

(** {2 IO} *)

type 'a printer = Format.formatter -> 'a -> unit

let rec pp_expr out = function
  | E_flag s -> Format.fprintf out "flag(%s)" s
  | E_test (n,s) -> Format.fprintf out "%s(%s)" n s
  | E_true -> Format.fprintf out "true"
  | E_false -> Format.fprintf out "false"
  | E_and (a,b) ->
    Format.fprintf out "(%a@ && %a)" pp_expr a pp_expr b
  | E_or (a,b) ->
    Format.fprintf out "(%a@ || %a)" pp_expr a pp_expr b
  | E_not e -> Format.fprintf out "!%a" pp_expr e

let rec pp_list pp out = function
  | [] -> ()
  | [x] -> pp out x
  | x :: tail ->
    Format.fprintf out "%a@,%a" pp x (pp_list pp) tail

let pp_str out = function
  | "" -> Format.pp_print_string out "."
  | s -> Format.pp_print_string out s

let rec unroll_if_ = function
  | [S_if (a,b,c)] ->
    let elif, else_ = unroll_if_ c in
    (a,b) :: elif, else_
  | st -> [], st

let rec pp_stmt out s =
  match s with
    | S_field (n, F_set l) ->
      Format.fprintf out "@[%s: @[<v>%a@]@]" n
        (pp_list pp_str) l
    | S_field (n, F_add l) ->
      Format.fprintf out "@[%s+: @[<v>%a@]@]" n
        (pp_list pp_str) l
    | S_field (n, F_eval e) ->
      Format.fprintf out "@[%s$: @[<h>%a@]@]" n pp_expr e
    | S_if (e, a, b) ->
      let elif, else_ = unroll_if_ b in
      begin match elif with
        | [] ->
          Format.fprintf out "@[<v>if @[<h>%a@]@ @[<2>  %a@]%a@]"
            pp_expr e (pp_list pp_stmt) a pp_else else_
        | _ ->
          let ppelif out (a,b) =
            Format.fprintf out "else if @[<h>%a@]@ @[<2>  %a@]"
              pp_expr a (pp_list pp_stmt) b
          in
          Format.fprintf out "@[<v>if @[<h>%a@]@ @[<2>  %a@]@ %a%a@]"
            pp_expr e (pp_list pp_stmt) a (pp_list ppelif) elif pp_else else_
      end

and pp_else out st_l = match st_l with
  | [] -> ()
  | _::_ ->
    Format.fprintf out "@ else@ @[<2>  %a@]" (pp_list pp_stmt) st_l

let pp_top_stmt out st =
  let pp_decl out d =
    Format.fprintf out "%s"
      (match d with
        | Flag -> "Flag"
        | Library -> "Library"
        | Object -> "Object"
        | Executable -> "Executable"
        | Source_repository -> "Source_repository"
        | Test -> "Test"
        | Document -> "Document"
      )
  in match st with
    | TS_stmt (S_if _ as s) ->
      Format.fprintf out "@[%a@]@," pp_stmt s
    | TS_stmt s -> pp_stmt out s
    | TS_decl (d,n,l) ->
      Format.fprintf out "@[<v2>%a %s@ %a@]@,"
        pp_decl d n (pp_list pp_stmt) l
