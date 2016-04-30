
(* This file is free software. See file "license" for more details. *)

(** {1 Very Simple Parser Combinators} *)

type line_num = int
type col_num = int

module H = Hashtbl.Make(struct
  type t = int * int  (* id of parser, position *)
  let equal ((a,b):t)(c,d) = a=c && b=d
  let hash = Hashtbl.hash
end)

type memo_ = (unit -> unit) H.t lazy_t

type position = int * int * int (* pos, line, column *)

type parse_branch = (line_num * col_num * string) list

type state = {
  str: string; (* the input *)
  mutable i: int; (* offset *)
  mutable lnum : line_num; (* Line number *)
  mutable cnum : col_num; (* Column number *)
  mutable branch: parse_branch;
  memo : memo_; (* Memoization table, if any *)
}

exception ParseError of parse_branch * (unit -> string)

let rec pp_branch () = function
  | [] -> ""
  | [l,c,s] ->
    Format.sprintf "@[at line %d, col %d%s@]" l c s
  | (l,c,s) :: tail ->
    Format.sprintf "@[at line %d, col %d%s@]@,%a" l c s pp_branch tail

let () = Printexc.register_printer
    (function
      | ParseError (b,msg) ->
        Some (Format.sprintf "@[<v>%s@ %a@]" (msg()) pp_branch b)
      | _ -> None)

let const_ x () = x

let state_of_string str =
  let s = {
    str;
    i=0;
    lnum=1;
    cnum=1;
    branch=[];
    memo=lazy (H.create 32);
  } in
  s

let is_done st = st.i = String.length st.str
let cur st = st.str.[st.i]

let fail_ ~err st msg =
  let b = (st.lnum, st.cnum, "") :: st.branch in
  err (ParseError (b, msg))

let next st ~ok ~err =
  if st.i = String.length st.str
  then fail_ st ~err (const_ "unexpected EOI")
  else (
    let c = st.str.[st.i] in
    st.i <- st.i + 1;
    ( if c='\n'
      then (st.lnum <- st.lnum + 1; st.cnum <- 1)
      else st.cnum <- st.cnum + 1
    );
    ok c
  )

let pos st = st.i, st.lnum, st.cnum

let backtrack st (i',l',c') =
  assert (0 <= i' && i' <= st.i);
  st.i <- i';
  st.lnum <- l';
  st.cnum <- c';
  ()

let sub st j len =
  assert (j + len <= st.i);
  String.sub st.str j len

type 'a t = state -> ok:('a -> unit) -> err:(exn -> unit) -> unit

let return : 'a -> 'a t = fun x _st ~ok ~err:_ -> ok x
let pure = return
let (>|=) : 'a t -> ('a -> 'b) -> 'b t
  = fun p f st ~ok ~err -> p st ~err ~ok:(fun x -> ok (f x))
let (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  = fun p f st ~ok ~err -> p st ~err ~ok:(fun x -> f x st ~err ~ok)
let (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  = fun f x st ~ok ~err ->
      f st ~err ~ok:(fun f' -> x st ~err ~ok:(fun x' -> ok (f' x')))
let (<* ) : 'a t -> _ t -> 'a t
  = fun x y st ~ok ~err ->
    x st ~err ~ok:(fun res -> y st ~err ~ok:(fun _ -> ok res))
let ( *>) : _ t -> 'a t -> 'a t
  = fun x y st ~ok ~err ->
    x st ~err ~ok:(fun _ -> y st ~err ~ok)

let junk_ st = next st ~err:(fun _ -> assert false) ~ok:ignore

let eoi st ~ok ~err =
  if is_done st
  then ok ()
  else fail_ ~err st (const_ "expected EOI")

let fail msg st ~ok:_ ~err = fail_ ~err st (const_ msg)
let failf msg = Printf.ksprintf fail msg

let parsing s p st ~ok ~err =
  st.branch <- (st.lnum, st.cnum, ", parsing " ^s) :: st.branch;
  p st
    ~ok:(fun x -> st.branch <- List.tl st.branch; ok x)
    ~err:(fun e -> st.branch <- List.tl st.branch; err e)

let nop _ ~ok ~err:_ = ok()

let char c =
  let msg = Printf.sprintf "expected '%c'" c in
  fun st ~ok ~err ->
    next st ~err
      ~ok:(fun c' -> if c=c' then ok c else fail_ ~err st (const_ msg))

let char_if p st ~ok ~err =
  next st ~err
    ~ok:(fun c ->
      if p c then ok c
      else fail_ ~err st (fun () -> Printf.sprintf "unexpected char '%c'" c)
    )

let chars_if p st ~ok ~err:_ =
  let i = st.i in
  let len = ref 0 in
  while not (is_done st) && p (cur st) do junk_ st; incr len done;
  ok (sub st i !len)

let chars1_if p st ~ok ~err =
  chars_if p st ~err
    ~ok:(fun s ->
      if s = ""
      then fail_ ~err st (const_ "unexpected sequence of chars")
      else ok s
    )

let rec skip_chars p st ~ok ~err =
  if not (is_done st) && p (cur st) then (
    junk_ st;
    skip_chars p st ~ok ~err
  ) else ok()

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
let is_num = function '0' .. '9' -> true | _ -> false
let is_alpha_num = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false
let is_space = function ' ' | '\t' -> true | _ -> false
let is_white = function ' ' | '\t' | '\n' -> true | _ -> false
let (~~~) p c = not (p c)
let (|||) p1 p2 c = p1 c || p2 c
let (&&&) p1 p2 c = p1 c && p2 c

let endline = char '\n'
let space = char_if is_space
let white = char_if is_white

let skip_space = skip_chars is_space
let skip_white = skip_chars is_white

let (<|>) : 'a t -> 'a t -> 'a t
  = fun x y st ~ok ~err ->
    let i = st.i in
    x st ~ok
      ~err:(fun e ->
        let j = st.i in
        if i=j then y st ~ok ~err (* try [y] *)
        else err e (* fail *)
      )

let try_ : 'a t -> 'a t
  = fun p st ~ok ~err ->
    let i = pos st in
    p st ~ok
      ~err:(fun e -> backtrack st i; err e)

let suspend f st ~ok ~err = f () st ~ok ~err

let (<?>) : 'a t -> string -> 'a t
  = fun x msg st ~ok ~err ->
    let i = st.i in
    x st ~ok
      ~err:(fun e ->
        if st.i = i
        then fail_ ~err st (fun () -> msg)
        else err e)

let string s st ~ok ~err =
  let rec check i =
    if i = String.length s then ok s
    else
      next st ~err
        ~ok:(fun c ->
          if c = s.[i]
          then check (i+1)
          else fail_ ~err st (fun () -> Printf.sprintf "expected \"%s\"" s))
  in
  check 0

let rec many_rec : 'a t -> 'a list -> 'a list t = fun p acc st ~ok ~err ->
  if is_done st then ok(List.rev acc)
  else
    p st ~err
      ~ok:(fun x ->
        let i = pos st in
        many_rec p (x :: acc) st ~ok
          ~err:(fun _ ->
            backtrack st i;
            ok(List.rev acc)
          )
      )

let many : 'a t -> 'a list t
  = fun p st ~ok ~err -> many_rec p [] st ~ok ~err

let many1 : 'a t -> 'a list t =
  fun p st ~ok ~err ->
    p st ~err ~ok:(fun x -> many_rec p [x] st ~err ~ok)

let rec skip p st ~ok ~err =
  let i = pos st in
  p st
    ~ok:(fun _ -> skip p st ~ok ~err)
    ~err:(fun _ ->
      backtrack st i;
      ok()
    )

(* by (sep1 ~by p) *)
let rec sep_rec ~by p = try_ by *> sep1 ~by p

and sep1 ~by p =
  p >>= fun x ->
  (sep_rec ~by p >|= fun tl -> x::tl)
  <|> return [x]

let sep ~by p =
  (try_ p >>= fun x ->
     (sep_rec ~by p >|= fun tl -> x::tl)
     <|> return [x])
  <|> return []

module MemoTbl = struct
  (* table of closures, used to implement universal type *)
  type t = memo_

  let create n = lazy (H.create n)

  (* unique ID for each parser *)
  let id_ = ref 0

  type 'a res =
    | Fail of exn
    | Ok of 'a
end

let fix f =
  let rec p st ~ok ~err = f p st ~ok ~err in
  p

let memo (type a) (p:a t):a t =
  let id = !MemoTbl.id_ in
  incr MemoTbl.id_;
  let r = ref None in (* used for universal encoding *)
  fun st ~ok ~err ->
    let i = st.i in
    let (lazy tbl) = st.memo in
    try
      let f = H.find tbl (i,id) in
      (* extract hidden value *)
      r := None;
      f ();
      begin match !r with
        | None -> assert false
        | Some (MemoTbl.Ok x) -> ok x
        | Some (MemoTbl.Fail e) -> err e
      end
    with Not_found ->
      (* parse, and save *)
      p st
        ~err:(fun e ->
          H.replace tbl (i,id) (fun () -> r := Some (MemoTbl.Fail e));
          err e
        )
        ~ok:(fun x ->
          H.replace tbl (i,id) (fun () -> r := Some (MemoTbl.Ok x));
          ok x
        )

let fix_memo f =
  let rec p =
    let p' = lazy (memo p) in
    fun st ~ok ~err -> f (Lazy.force p') st ~ok ~err
  in
  p

let get_lnum = fun st ~ok ~err:_ -> ok st.lnum
let get_cnum = fun st ~ok ~err:_ -> ok st.cnum
let get_pos = fun st ~ok ~err:_ -> ok (st.lnum, st.cnum)

let parse st ~p =
  let res = ref None in
  p st ~ok:(fun x -> res := Some x) ~err:(fun e -> raise e);
  match !res with
    | None -> assert false
    | Some x -> x

let parse_string s ~p = parse (state_of_string s) ~p

let read_all_ ic =
  let buf = Buffer.create 256 in
  begin
    try
      while true do
        let line = input_line ic in
        Buffer.add_string buf line;
        Buffer.add_char buf '\n';
      done;
      assert false
    with End_of_file -> ()
  end;
  Buffer.contents buf

let parse_file ~file ~p =
  let ic = open_in file in
  let st = state_of_string (read_all_ ic) in
  try
    let res = parse st ~p in
    close_in ic;
    res
  with e ->
    close_in ic;
    raise e

module Infix = struct
  let (>|=) = (>|=)
  let (>>=) = (>>=)
  let (<*>) = (<*>)
  let (<* ) = (<* )
  let ( *>) = ( *>)
  let (~~~) = (~~~)
  let (|||) = (|||)
  let (&&&) = (&&&)
  let (<|>) = (<|>)
  let (<?>) = (<?>)
end
