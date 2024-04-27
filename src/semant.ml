(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

let check (globals, structs, functions) =
  let check_binds (kind : string) (binds : (typ * string) list) =
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (* Make sure no globals duplicate *)
  check_binds "global" globals;

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    StringMap.add "print" {
      rtyp = Int;
      fname = "print";
      formals = [(Int, "x")];
      locals = []; body = [] } StringMap.empty
      (* TODO: Add more built in functions later *)
  in

  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
      _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ ->  StringMap.add n fd map
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in
  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_fun func = 
      check_binds "arg" func.args;
      check_binds "local" func.locals;

      let add_symbol m (ty, name) = StringMap.add name ty m 
      in 
      let symbols = List.fold_left add_symbol StringMap.empty (globals @ func.args @ func.locals)
      in 
      (* Return a variable from our local symbol table *)
      let type_of_identifier s =
        try StringMap.find s symbols
        with Not_found -> raise (Failure ("undeclared identifier " ^ s))
      in 
      let rec check_stmt_list l = 
      match l with 
      [] -> [] 
      | s :: sl -> check_stmt s :: check_stmt_list sl
      and check_stmt s = 
        match s with 
        Block sl -> SBlock(check_stmt_list sl)

      in 

      { srtyp=func.rtype
      ; sfname=func.fname
      ; sargs=func.args
      ; slocals=func.locals
      ; sbody=check_stmt_list func.body
      ; sreturn=func.return
      }
in 
(globals, structs, List.map check_fun functions)