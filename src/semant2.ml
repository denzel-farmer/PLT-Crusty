(* Semantic checking for the Crusty compiler *)

open Ast
open Sast
open Astprint

module StringMap = Map.Make(String)

let check program =
  let check_fun func = 
    { 
      srtyp=func.rtyp;
      sfname=func.fname;
      sargs=func.args;
      slocals=func.locals;
      sbody=[];
      sreturn=SVoidReturn
    }
in
(program.globals, program.structs, List.map check_fun program.funcs)