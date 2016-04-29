
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
  P.skip_chars P.is_space *> P.endline

let single_dot_line : char P.t =
  P.skip_chars P.is_space *> P.char '.' *> P.skip_chars P.is_space *> P.endline

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
         <|>
         ((atomic <* P.skip_space) <?> "expected expression"
          >>= fun l ->
           ( parse_binop >>= fun op ->
             P.skip_space *> self >|= fun r ->
             match op with `And -> A.e_and l r | `Or -> A.e_or l r)
          <|> P.return l))

(* parse a block of strings at given indentation.
   @param acc the current list of lines *)
let rec parse_block_rec indent acc : string P.t =
  P.get_cnum >>= fun i -> assert (i=indent);
  (empty_line >|= fun _ -> String.concat "\n" (List.rev acc))
  <|>
    (((P.chars1_if (fun c->c <> '\n') <* P.endline) <?> "expected line")
    >>= fun line ->
    let line = if line = "." then "" else line in
    parse_block_rec indent (line :: acc))

type block_type =
  | BT_same_line
  | BT_new_line of int

(* entry point for parsing a multiline block.
   if the current line is empty, it checks that the next line
   has indentation > indent;
   else it starts reading the block on the same line *)
let parse_block indent : string P.t =
  ( (empty_line *> parse_indent >>= fun i' ->
    if i' <= indent then P.fail "wrong indentation, expected indented block"
    else P.return (BT_new_line i'))
    <|> (P.skip_space *> P.return BT_same_line))
  >>= function
  | BT_same_line -> parse_block_rec indent []
  | BT_new_line indent' -> parse_block_rec indent' []

type field_sep =
  | FS_colon
  | FS_add
  | FS_eval

let parse_field_sep : field_sep P.t =
  (P.string ":" *> P.return FS_colon)
  <|> (P.string "+:" *> P.return FS_add)
  <|> (P.string "$:" *> P.return FS_eval)
  <?> "expected `:`, `$:` or `+:`"

(* parse a "if" statement *)
let rec parse_if indent : A.stmt P.t =
  P.string "if" *> parse_expr <* eat_line
  >>= fun a ->
  parse_indent >>= fun i1 ->
  if i1 <= indent then P.failf "after `if`, expected indent > %d" indent
  else parse_stmt i1 <* eat_line >>= fun b ->
  parse_indent >>= fun i ->
  if i <> indent then P.failf "after `if`, expected `else` at level %d" indent
  else (
    eat_line *> parse_indent >>= fun i2 ->
    if i1<>i2 then P.failf "after `else`, expected consistent indent %d" i1
    else parse_stmt i1 <* eat_line >>= fun c -> P.return (A.s_if a b c)
  )

and parse_field indent : A.stmt P.t =
  P.chars1_if P.is_alpha_num <* P.skip_space >>= fun fname ->
  (parse_field_sep >>= function
    | FS_colon -> parse_block indent >|= fun b -> A.f_set b
    | FS_add -> parse_block indent >|= fun b -> A.f_add b
    | FS_eval -> parse_expr >|= fun e -> A.f_eval e
  ) >|= A.s_field fname

(* atomic statement: field/if *)
and parse_stmt indent : A.stmt P.t =
  parse_if indent
  <|> parse_field indent
  <|> P.fail "expected statement"

(* parse a "Library foo: blabla"-like statement *)
let parse_top_decl : A.toplevel_decl -> A.top_stmt P.t
  = fun decl ->
    parse_name <* P.skip_space >>= fun name ->
    P.get_lnum >>= fun indent ->
    P.many1 (parse_stmt indent) >|= fun l ->
    A.ts_decl decl name l

(* toplevel statements *)
let parse_top_stmt : A.top_stmt P.t =
  let st =
    P.chars1_if P.is_alpha_num
    >>= function
    | "if" -> parse_if 0 >|= A.ts_stmt
    | "Library" -> parse_top_decl A.Library
    | "Executable" -> parse_top_decl A.Executable
    | "Object" -> parse_top_decl A.Object
    | "Test" -> parse_top_decl A.Test
    | "SourceRepository" -> parse_top_decl A.Source_repository
    | "Document" -> parse_top_decl A.Document
    | s -> P.failf "unknown toplevel declaration `%s`" s
  in
  P.fix_memo
    (fun self -> st <|> (empty_line *> self))

let parse : A.top_stmt list P.t =
  P.many parse_top_stmt

let parse_string s = Oasis_parser_co.parse_string s ~p:parse
let parse_file f = Oasis_parser_co.parse_file ?size:None ~file:f ~p:parse


