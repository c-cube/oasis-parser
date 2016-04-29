
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

type field_op =
  | F_set of string  (* foo: bar *)
  | F_add of string  (* foo+: bar *)
  | F_eval of expr   (* foo$: bar *)

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


