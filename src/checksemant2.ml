let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Crustyparse.program Scanner.token lexbuf in
  let sprogram = Semant3.check program in
  print_endline (Sast.string_of_sprogram sprogram)