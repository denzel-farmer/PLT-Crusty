let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Crustyparse.program Scanner.token lexbuf in
  Semant2.check program 
  (* print_endline (Sast.string_of_sprogram sprogram) *)