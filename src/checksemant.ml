let _ =
  (* Log.set_log_level_debug; *)
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
  then (print_endline "\n LINEAR CHECK FAILED, EXITING";
  match Linear.lin_result_get_reason linear_result with
  | Some reason -> print_endline ("Reason: " ^ reason)
  | None -> raise (Failure "No reason given for linear check failure")
  )
  else (
    Log.info_println "\n Doing psuedo-translations...";
    let translated = Psuedotranslate.eliminate_psuedo_nodes sprogram in
    Log.debug_println (Sastprint.string_of_sprogram translated);
    print_endline "\nSEMANTIC COMPILATION SUCCESS")
;;
