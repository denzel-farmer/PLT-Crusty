(* Top-level of the Crusty compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, generate LLVM IR,
   and dump the module *)

open Lexing

type action = Ast | Sast | LLVM_IR

let colnum pos =
  (pos.pos_cnum - pos.pos_bol) - 1

let pos_string pos =
  let l = string_of_int pos.pos_lnum
  and c = string_of_int ((colnum pos) + 1) in
  "line " ^ l ^ ", column " ^ c
  
let () =
  let action = ref LLVM_IR in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
  ] in
  let usage_msg = "usage: ./crusty.native [-a|-s|-l] [file.mc]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  try
    let ast = Crustyparse.program Scanner.token lexbuf in
    match !action with
      Ast -> print_string (Astprint.string_of_program ast)
    | _ -> let sast = Semant.check ast in
      match !action with
        Ast     -> ()
      | Sast    -> print_string (Sast.string_of_sprogram sast)
      | LLVM_IR -> print_string ("NOT IMPLEMENTED") (* (Llvm.string_of_llmodule (Irgen.translate sast)) *)
  with 
      Crustyparse.Error -> raise (Failure ("Parse error at " ^ (pos_string lexbuf.lex_curr_p)))
    | Semant.Error s -> raise (Failure s)