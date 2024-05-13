let _ =
  let lexbuf = Lexing.from_channel stdin in
  Log.info_println "\nGenerating Abstract Syntax Tree";
  let program = Crustyparse.program Scanner.token lexbuf in
  Log.debug_println (Astprint.string_of_program program);
  Log.info_println "\nGenerating Semantically Checked Abstract Syntax Tree";
  let sprogram = Semant.check program in
  Log.debug_println (Sastprint.string_of_sprogram sprogram);
  Log.info_println "\nDoing Linearity Checks...";
  let linear_result = Linear.check sprogram in
  Log.debug_println (Linear.string_of_linear_result linear_result);
  if Linear.lin_result_failed linear_result
  then print_endline "\n[PART 1] COMPILATION FAILED, EXITING"
  else (
    Log.info_println "\n Doing psuedo-translations...";
    let translated = Psuedotranslate.eliminate_psuedo_nodes sprogram in
    Log.debug_println (Sastprint.string_of_sprogram translated);
    print_endline "\n[PART 1] COMPILATION SUCCESS";)
;;
