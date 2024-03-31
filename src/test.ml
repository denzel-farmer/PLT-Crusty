open Astprint

let _ = 
  let lexbuf = Lexing.from_channel stdin in
  let program = Crustyparse.program Scanner.token lexbuf in
  print_endline (string_of_program program)