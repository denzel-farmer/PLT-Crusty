(* Top-level of the MicroC compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, generate LLVM IR,
   and dump the module *)

type action =
  | Ast
  | Sast
  | LLVM_IR

let () =
  let action = ref LLVM_IR in
  let set_action a () = action := a in
  let speclist =
    [ "-a", Arg.Unit (set_action Ast), "Print the AST"
    ; "-s", Arg.Unit (set_action Sast), "Print the SAST"
    ; "-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR"
    ]
  in
  let usage_msg = "usage: ./crusty.native [-a|-s|-l] [file.mc]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;
  let lexbuf = Lexing.from_channel !channel in
  Log.info_println "\nGenerating Abstract Syntax Tree";
  let ast = Crustyparse.program Scanner.token lexbuf in
  Log.debug_println (Astprint.string_of_program ast);
  match !action with
  | Ast -> print_string (Astprint.string_of_program ast)
  | _ ->
    Log.info_println "\nGenerating Semantically Checked Abstract Syntax Tree";
    let sast = Semant.check ast in
    Log.debug_println "\nSAST:";
    Log.debug_println (Sastprint.string_of_sprogram sast);
    Log.info_println "\nDoing Linearity Checks...";
    let linear_result = Linear.check sast in
    Log.debug_println (Linear.string_of_linear_result linear_result);
    Log.debug_println "\nLinear Report:";
    Log.debug_println (Linear.string_of_linear_result linear_result);
    if Linear.lin_result_failed linear_result
    then print_endline "\Linearity Check: FAILED"
    else (
      Log.info_println "\nLinearity Check Passed";
      Log.info_println "\nDoing psuedo-translations...";
      let translated = Psuedotranslate.eliminate_psuedo_nodes sast in
      Log.debug_println "\nTranslated:";
      Log.debug_println (Sastprint.string_of_sprogram translated);
      match !action with
      | Ast -> ()
      | Sast -> print_string (Sastprint.string_of_sprogram sast)
      | LLVM_IR -> print_string (Llvm.string_of_llmodule (Irgen.translate translated)))
;;
