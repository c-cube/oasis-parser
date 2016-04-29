
(* This file is free software. See file "license" for more details. *)

(** {1 The parser} *)

module A = Oasis_ast
module P = Oasis_parser_co

open P.Infix

type 'a parser_ = 'a Oasis_parser_co.t

let print_pos (l,c) = Printf.sprintf "line %d, col %d" l c

(* print error message when a leading tab is met *)
let fail_tab (l,c) = P.failf "error: leading tab at %s\n%!" (print_pos (l,c))

(* parse leading indentation, return new indentation *)
let parse_indent : int P.t =
  P.chars_if P.is_space >>= fun s ->
  if String.contains s '\t'
  then
    P.get_pos >>= fun (l,c) ->
    fail_tab (l, c)
  else
    let len = String.length s in
    P.return len

let eat_line = P.skip_space <* P.endline

let empty_line : char P.t =
  P.chars_if P.is_space *> P.endline

let single_dot_line : char P.t =
  P.chars_if P.is_space *> P.char '.' *> P.chars_if P.is_space *> P.endline

let parse_name : A.name P.t =
  let quoted = P.char '"' *> P.chars_if (fun c->c <> '"') <* P.char '"' in
  let non_quoted = P.chars1_if P.is_alpha_num in
  quoted <|> non_quoted <|> P.fail "expected a name"

let parse_flag =
  P.string "flag" *> P.skip_space *> P.char '(' *> P.skip_space *>
    parse_name <* P.skip_space <* P.char ')'
  >|= A.e_flag

(* TODO: allow _ in test name *)

let parse_test =
  P.chars1_if P.is_alpha_num <* P.skip_space >>= fun t ->
  P.char '(' *> P.skip_space *> parse_name <* P.skip_space <* P.char ')' >|= fun v ->
  A.e_test t v

let parse_binop =
  (P.string "&&" *> P.return `And)
  <|> (P.string "||" *> P.return `Or)

let parse_expr : A.expr P.t =
  P.fix
    (fun self ->
       let atomic =
         (P.char '(' *> self <* P.skip_space <* P.char ')')
         <|> (P.string "true" *> P.return A.e_true)
         <|> (P.string "false" *> P.return A.e_false)
         <|> parse_flag
         <|> parse_test
       in
       P.skip_space *>
         (P.char '!' *> P.skip_space *> self)
         <|> (atomic <* P.skip_space >>= fun l ->
              parse_binop >>= fun op ->
              P.skip_space *> self >|= fun r ->
              match op with `And -> A.e_and l r | `Or -> A.e_or l r)
         <|> atomic
         <?> "expected expression")

(* parse a "if" statement *)
let rec parse_if indent : A.stmt P.t =
  P.string "if" *> parse_expr <* eat_line
  >>= fun a ->
  parse_indent >>= fun i1 ->
  if i1 <= indent then P.failf "after `if`, expected indent > %d" indent
  else parse_stmt i1 <* eat_line >>= fun b ->
  parse_indent >>= fun i ->
  if i <> indent then P.failf "after `if`, expected `else` at level %d" indent
  else
    eat_line *> parse_indent >>= fun i2 ->
    if i1<>i2 then P.failf "after `else`, expected consistent indent %d" i1
    else parse_stmt i1 <* eat_line >>= fun c -> P.return (A.s_if a b c)

(* parse a block of statements at given indentation *)
and parse_stmt_block indent : A.stmt P.t =
  assert false (* TODO *)

and parse_stmt indent : A.stmt P.t =
  parse_if indent
  <|> parse_stmt_block indent
  <|> P.fail "expected statement"

(* parse a "Library foo: blabla"-like statement *)
let parse_top_decl : A.toplevel_decl -> A.top_stmt P.t
  = fun decl ->
    parse_name <* P.skip_space >>= fun name ->
    P.get_lnum >>= fun indent ->
    parse_stmt_block indent >|= fun st ->
    A.ts_decl decl name st

(* toplevel statements *)
let parse_top_stmt : A.top_stmt P.t =
  let st =
    P.chars1_if P.is_alpha_num
    >>= function
    | "if" -> parse_if 0 >|= A.ts_stmt
    | "Library" -> parse_top_decl A.Library
    | "Executable" -> parse_top_decl A.Executable
    | s -> P.failf "unknown toplevel declaration `%s`" s
  in
  P.fix_memo
    (fun self -> st <|> (empty_line *> self))

let parse : A.top_stmt list P.t =
  P.many parse_top_stmt

let parse_string s = Oasis_parser_co.parse_string s ~p:parse
let parse_file f = Oasis_parser_co.parse_file ?size:None ~file:f ~p:parse


