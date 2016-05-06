
(* This file is free software. See file "license" for more details. *)

(** {1 Abstract Syntax Tree} *)

type name = string
type flag = string
type test = string

type expr =
  | E_flag of flag
  | E_test of test * string
  | E_true
  | E_false
  | E_and of expr * expr
  | E_or of expr * expr
  | E_not of expr

val e_flag : flag -> expr
val e_test : test -> string -> expr
val e_true : expr
val e_false : expr
val e_and : expr -> expr -> expr
val e_or : expr -> expr -> expr
val e_not : expr -> expr

type field_op = F_set of string list | F_add of string list | F_eval of expr

val f_set : string list -> field_op
val f_add : string list -> field_op
val f_eval : expr -> field_op

type stmt =
  | S_field of name * field_op
  | S_if of expr * stmt list * stmt list

val s_field : name -> field_op -> stmt
val s_if : expr -> stmt list -> stmt list -> stmt

val s_if_l :
  expr ->
  then_:stmt list ->
  elif:(expr * stmt list) list ->
  else_:stmt list ->
  stmt

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

val ts_decl : toplevel_decl -> name -> stmt list -> top_stmt
val ts_stmt : stmt -> top_stmt

(** {2 IO} *)

type 'a printer = Format.formatter -> 'a -> unit

val pp_expr : expr printer
val pp_stmt : stmt printer
val pp_top_stmt : top_stmt printer

