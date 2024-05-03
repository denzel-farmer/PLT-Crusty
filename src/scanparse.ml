(* open Astprint

let _ = 
  let lexbuf = Lexing.from_channel stdin in
  let program = Crustyparse.program Scanner.token lexbuf in
  print_endline (string_of_program program) *)

open Lexing
open Astprint

let colnum pos =
  (pos.pos_cnum - pos.pos_bol) - 1

let pos_string pos =
  let l = string_of_int pos.pos_lnum
  and c = string_of_int ((colnum pos) + 1) in
  "line " ^ l ^ ", column " ^ c

let parse' f =
  let lexbuf = Lexing.from_channel stdin in
  try
    f Scanner.token lexbuf
  with Crustyparse.Error ->
    raise (Failure ("Parse error at " ^ (pos_string lexbuf.lex_curr_p)))

let _ = 
  let program = parse' Crustyparse.program in
  print_endline (string_of_program program)
