
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
let parse_indent : int P.t = P.skip_space *> P.get_cnum

let eat_comment =
  (P.try_ (P.char '#') *> P.skip_chars (fun c->c <> '\n')) <|> P.nop

let empty_line : char P.t = P.skip_space *> eat_comment *> P.endline

(* char belonging to a name *)
let is_name_ = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true
  | _ -> false

let parse_name : A.name P.t =
  let quoted = P.try_ (P.char '"') *> P.chars_if (fun c->c <> '"') <* P.char '"' in
  let non_quoted = P.chars1_if is_name_ in
  quoted <|> non_quoted <?> "expected a name"

let parse_flag =
  P.try_ (P.string "flag") *> P.skip_space *> P.char '(' *> P.skip_space *>
    parse_name <* P.skip_space <* P.char ')'
  >|= A.e_flag

let parse_test =
  P.try_ (P.chars1_if P.is_alpha_num) <* P.skip_space >>= fun t ->
  P.char '(' *> P.skip_space *> parse_name <* P.skip_space <* P.char ')' >|= fun v ->
  A.e_test t v

let parse_binop =
  (P.try_ (P.string "&&") *> P.return `And)
  <|> (P.try_ (P.string "||") *> P.return `Or)

let parse_expr : A.expr P.t =
  P.fix_memo
    (fun self ->
       let atomic =
         (P.try_ (P.char '(') *> self <* P.skip_space <* P.char ')')
         <|> (P.try_ (P.string "true") *> P.return A.e_true)
         <|> (P.try_ (P.string "false") *> P.return A.e_false)
         <|> parse_flag
         <|> parse_test
       in
       P.skip_space *> (
         (P.try_ (P.char '!') *> P.skip_space *> self >|= A.e_not)
         <|>
         (atomic <* P.skip_space
          >>= fun l ->
           ( parse_binop <* P.skip_space >>= fun op ->
             self >|= fun r ->
             match op with `And -> A.e_and l r | `Or -> A.e_or l r)
           <|> P.return l)
         <?> "expected expression"
       ))

(* parse a block of strings at given indentation.
   @param acc the current list of lines *)
let rec parse_block_rec indent acc : string list P.t =
  parse_indent >>= fun i ->
  if i<indent then P.return (List.rev acc) (* dedent *)
  else (
    (* same indent, try to parse another line *)
    (P.try_ P.endline >|= fun _ -> List.rev acc)
    <|>
    (
      ((P.chars1_if (fun c->c <> '\n') <* P.endline) <?> "expected non-empty line")
      >>= fun line ->
      let line = String.trim line in
      (* line with only a ".": replace with blank line *)
      let line = if line = "." then "" else line in
      parse_block_rec indent (line :: acc)
    )
  )

(* entry point for parsing a multiline block.
   if the current line is empty, it checks that the next line
   has indentation > indent;
   else it starts reading the block on the same line *)
let parse_block indent : string list P.t =
  (P.try_ P.endline *> parse_indent >>= fun i' ->
   if i' <= indent
   then P.failf "wrong indentation, expected indented block > %d, got %d" indent i'
   else parse_block_rec i' []
  )
  <|>
    (parse_indent >>= fun indent' ->
     assert (indent' >= indent);
     parse_block_rec indent' [])

type field_sep =
  | FS_colon
  | FS_add
  | FS_eval

let parse_field_sep : field_sep P.t =
  (P.try_ (P.string ":") *> P.return FS_colon)
  <|> (P.try_ (P.string "+:") *> P.return FS_add)
  <|> (P.try_ (P.string "$:") *> P.return FS_eval)
  <?> "expected `:`, `$:` or `+:`"

(* parse a "if" statement *)
let rec parse_if indent : A.stmt P.t =
  P.try_ (P.string "if") *> parse_expr <* empty_line
  >>= fun a ->
  parse_indent >>= fun i1 ->
  if i1 <= indent then P.failf "after `if`, expected indent > %d" indent
  else parse_stmts i1 [] >>= fun b ->
  parse_indent >>= fun i ->
  if i > indent then P.failf "after `if`, expected `else` at level %d" indent
  else (
    (
      P.try_ (P.string "else") *> empty_line *> parse_indent >>= fun i2 ->
      if i1<>i2 then P.failf "after `else`, expected consistent indent %d" i1
      else parse_stmts i1 [] >>= fun c -> P.return (A.s_if a b c)
    )
    <|> P.return (A.s_if a b []) (* no 'else' *)
  )

and parse_field : A.stmt P.t =
  parse_indent >>= fun indent ->
  P.try_ (P.chars1_if P.is_alpha_num) <* P.skip_space >>= fun fname ->
  (parse_field_sep >>= function
    | FS_colon -> parse_block indent >|= fun b -> A.f_set b
    | FS_add -> parse_block indent >|= fun b -> A.f_add b
    | FS_eval -> parse_expr >|= fun e -> A.f_eval e
  ) >|= A.s_field fname

(* atomic statement: field/if *)
and parse_stmt indent : A.stmt P.t =
  parse_indent >>= fun indent' ->
  if indent=indent' then (
    parse_if indent
    <|> parse_field
    <?> "expected statement"
  ) else P.failf "parse_stmt: wrong indentation (expected %d, got %d)" indent indent'

and parse_stmts indent acc =
  P.skip empty_line *> P.skip_space *>
  parse_indent >>= fun indent' ->
  if indent' < indent then
    if acc<>[]
    then P.return (List.rev acc)
    else P.fail "expected non-empty list of statements"
  else (
    parse_stmt indent >>= fun st ->
    parse_stmts indent (st :: acc)
  )

(* parse a "Library foo: blabla"-like statement *)
let parse_top_decl : A.toplevel_decl -> A.top_stmt P.t
  = fun decl ->
    P.skip_space *> parse_name <* P.skip_space >>= fun name ->
    P.endline *> parse_indent >>= fun indent ->
    if indent=1 then P.fail "expected indented list of statements"
    else (
      parse_stmts indent [] >|= fun l ->
      A.ts_decl decl name l
    )

(* toplevel statements *)
let parse_top_stmt : A.top_stmt P.t =
  let parse_decl =
    P.try_ (
      P.chars1_if P.is_alpha_num >>= function
      | "Flag" -> P.return A.Flag
      | "Library" -> P.return A.Library
      | "Executable" -> P.return A.Executable
      | "Object" -> P.return A.Object
      | "Test" -> P.return A.Test
      | "SourceRepository" -> P.return A.Source_repository
      | "Document" -> P.return A.Document
      | _ -> P.fail "expected decl"
    ) >>= parse_top_decl
  in
  parse_indent >>= fun indent ->
  if indent<>1 then P.fail "toplevel statement cannot be indented"
  else (
    parse_decl
    <|> (parse_stmt indent >|= A.ts_stmt)
  )

(* parse a list of toplevel statements *)
let parse : A.top_stmt list P.t =
  let rec aux acc =
    P.skip empty_line *> (
      (P.try_ (P.skip_space *> P.eoi) >|= fun _ -> List.rev acc)
      <|> (parse_top_stmt >>= fun s -> aux (s :: acc))
    )
  in
  aux []

let parse_string s = Oasis_parser_co.parse_string s ~p:parse
let parse_file f = Oasis_parser_co.parse_file ~file:f ~p:parse


