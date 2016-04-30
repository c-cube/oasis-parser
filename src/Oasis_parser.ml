
(* This file is free software. See file "license" for more details. *)

(** {1 The parser} *)

module A = Oasis_ast
module P = Oasis_parser_co

open P.Infix

type 'a parser_ = 'a Oasis_parser_co.t

let print_pos (l,c) = Printf.sprintf "line %d, col %d" l c

(* print error message when a leading tab is met *)
let fail_tab (l,c) = P.failf "error: leading tab at %s\n%!" (print_pos (l,c))

let comment = P.try_ (P.char '#') *> P.skip_chars (fun c->c <> '\n')

let rec next_block () =
  P.skip_space *>
  ( (comment *> P.endline *> P.suspend next_block)
    <|> P.nop
  )

(* parse leading indentation, return new indentation *)
let parse_indent : int P.t = next_block () *> P.get_cnum

let eat_comment = comment <|> P.nop
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
  P.try_ parse_name <* P.skip_space >>= fun t ->
  P.char '(' *> P.skip_space *> parse_name <* P.skip_space <* P.char ')' >|= fun v ->
  A.e_test t v

let parse_binop =
  (P.try_ (P.string "&&") *> P.return `And)
  <|> (P.try_ (P.string "||") *> P.return `Or)

let suspend_ s p = P.parsing s (P.suspend p)

let parse_expr : A.expr P.t =
  let rec atomic () =
    (P.try_ (P.char '(') *> suspend_ "expr" top <* P.skip_space <* P.char ')')
    <|> (P.try_ (P.string "true") *> P.return A.e_true)
    <|> (P.try_ (P.string "false") *> P.return A.e_false)
    <|> parse_flag
    <|> parse_test
    <?> "expected atomic expression"
  and unary() =
    suspend_ "atomic expr" atomic
    <|> (P.try_ (P.char '!') *> P.skip_space *> P.suspend unary >|= A.e_not)
    <?> "expected expression"
  and top () =
    P.skip_space *> P.suspend unary <* P.skip_space >>= fun l ->
    ( parse_binop <* P.skip_space >>= fun op ->
      P.suspend top >|= fun r ->
      match op with `And -> A.e_and l r | `Or -> A.e_or l r)
    <|> P.return l
  in
  suspend_ "expr" top

let rest_of_line : string P.t =
  (P.chars1_if (fun c->c <> '\n') <* P.endline) <?> "expected non-empty line"

(* parse a block of strings at given indentation.
   @param acc the current list of lines *)
let rec parse_block_rec indent acc : string list P.t =
  parse_indent >>= fun i ->
  (* skip empty lines *)
  (eat_comment *> P.try_ P.endline *> parse_block_rec indent acc)
  <|>
  ( if i<indent then P.return (List.rev acc) (* dedent *)
    else (
      (* same indent, try to parse another line *)
      rest_of_line >|= String.trim >>= fun line ->
      (* line with only a ".": replace with blank line *)
      let line = if line = "." then "" else line in
      parse_block_rec indent (line :: acc)
    )
  )

(* entry point for parsing a multiline block.
   if the current line is empty, it checks that the next line
   has indentation > indent and parses a block of same indentation;
   else it reads it and tries to read an indented block on next lines *)
let parse_block indent : string list P.t =
  P.skip_space *> eat_comment *>
  ( (* indented block after the newline *)
    ( P.try_ P.endline *> parse_indent >>= fun i' ->
      if i' <= indent
      then P.fail "expected non-empty indented block"
      else P.parsing "indented block" (parse_block_rec i' []))
    <|>
    (* start block on same line, maybe continue it after indentation *)
    ( P.try_ rest_of_line >|= String.trim >>= fun x ->
      parse_indent >>= fun i' ->
      if i' <= indent
      then P.return [x]
      else P.parsing "indented block" (parse_block_rec i' [x]))
    <?> "expected indented block"
  )

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
  P.try_ (P.string "if") *> parse_expr <* empty_line >>= fun e ->
  parse_cond_branch indent >>= fun a ->
  parse_elses indent >|= fun (l, c) -> A.s_if_l e a l c

and parse_cond_branch indent =
  parse_indent >>= fun sub_i ->
  if sub_i <= indent then P.failf "after `if` or `else if`, expected indent > %d" indent
  else parse_stmts sub_i []

and parse_elses indent =
  ( P.try_ (P.string "else") >>= fun _ ->
    (* either simple "else" , or "else if" *)
    ( P.try_ empty_line *> parse_indent *>
      parse_cond_branch indent >|= fun c -> [], c)
    <|>
    ( P.skip_space *> P.string "if" *> P.skip_space *> parse_expr <* empty_line
      >>= fun a' ->
      parse_cond_branch indent
      >>= fun b' ->
      parse_elses indent >|= fun (l,c) -> (a',b') :: l, c)
  )
  <|> P.return ([], []) (* no 'else' *)

and parse_field indent : A.stmt P.t =
  P.try_ (P.chars1_if P.is_alpha_num) <* P.skip_space >>= fun fname ->
  (parse_field_sep >>= function
    | FS_colon -> parse_block indent >|= fun b -> A.f_set b
    | FS_add -> parse_block indent >|= fun b -> A.f_add b
    | FS_eval -> parse_expr >|= fun e -> A.f_eval e
  ) >|= A.s_field fname

(* atomic statement: field/if *)
and parse_stmt indent : A.stmt P.t =
  parse_indent >>= fun indent' ->
  if indent=indent'
  then (
        P.parsing "if statement" (parse_if indent)
    <|> P.parsing "field statement" (parse_field indent)
    <?> "expected statement"
  ) else P.failf "parse_stmt: wrong indentation (expected %d, got %d)" indent indent'

and parse_stmts indent acc =
  P.skip empty_line *> parse_indent >>= fun indent' ->
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
      P.parsing "indented statements" (parse_stmts indent []) >|= fun l ->
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
  else P.parsing "top statement" (
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

let parse_string s = P.parse_string s ~p:parse
let parse_file f = P.parse_file ~file:f ~p:parse


