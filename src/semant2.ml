(* Semantic checking for the Crusty compiler *)

open Ast
open Sast
open Astprint

module StringMap = Map.Make(String)

let check program =
  let check_binds_dup (kind: string) (binds : var_decl list) =
    let check_member n bs = 
      List.fold_left (fun flag (_, n') -> if n = n' then true else flag) false bs (* why three parameters here? for linearity?*)
  in 
    let rec check_dup bs = 
      match bs with 
      | [] -> () 
      | (_, n) :: sl -> if check_member n sl then raise (Failure ("duplicate variable " ^ n ^ " in " ^ kind )) else check_dup sl
    in check_dup binds
  in 
  
  check_binds_dup "global" program.globals;

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = (* how do we import built in functions? *)
    StringMap.add "print" {
      rtyp = Nonvoid(Prim(Unrestricted, Int));
      fname = "print";
      args = [(Prim(Unrestricted, Int), "x")];
      locals = []; body = []; return = Return(Literal(IntLit(1)))} StringMap.empty
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
  let function_decls = List.fold_left add_func built_in_decls program.funcs
  in
  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in
  let _ = find_func "main" in (* Ensure "main" is defined *)

  let add_struct map (st: struct_def) = 
    let dup_err = "duplicate struct " ^ st.sname
    and make_err er = raise (Failure er)
    and n = st.sname (* Name of the function *)
    in match st with (* No duplicate functions or redefinitions of built-ins *)
    | _ when StringMap.mem n map -> make_err dup_err
    | _ ->  
      (* Make sure no duplicate fields in same struct*) (* Can probably make a helper function for all of these dup checks? *)
      let add_struct_field map (field: var_decl) =
        let make_err er = raise (Failure er)
        and field_name = match field with 
        | (t, s) -> s
        in let dup_err = "duplicate field " ^ field_name
        in match field_name with 
        | _ when StringMap.mem n map -> make_err dup_err
        | _ ->  StringMap.add n field map
      in
      List.fold_left add_struct_field StringMap.empty st.fields;
      StringMap.add n st map;
  in
  
  let check_fun func = 
    check_binds_dup "args" func.args;
    check_binds_dup "locals" func.locals;

    (* Raise an exception if the given rvalue type cannot be assigned to
    the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    let add_symbol m (ty, name) = StringMap.add name ty m 
    in 
    let symbols = List.fold_left add_symbol StringMap.empty (program.globals @ func.args @ func.locals)
    in 

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Check literals*)
    let check_literal = function 
        IntLit l -> (Prim(Unrestricted, Int), SIntLit l)
      | BoolLit l -> (Prim(Unrestricted, Bool), SBoolLit l)
      | CharLit l -> (Prim(Unrestricted, Char), SCharLit l)
      | FloatLit l -> (Prim(Unrestricted, Float), SFloatLit l)
      | StructLit l -> (Struct(), SStructLit l) (* fill in struct type? *)
      | StringLit l -> (Prim(Unrestricted, String), SStringLit l)
      | ArrayLit l -> (Arr() , SArrayLit l) (* fill in arr type? *)
    in 
    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr = function
      | Id var -> (type_of_identifier var, SId var)
      | Literal l -> check_literal l 
      | Assignment l -> check_all_assignment l
      | Operation l -> check_operation l
      | Call (fname, args) as call -> 
        let fd = find_func fname in
        let param_length = List.length fd.args in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
        else let check_call (ft, _) e =
               let (et, e') = check_expr e in
               let err = "illegal argument found " ^ string_of_typ et ^
                         " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
               in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.rtyp, SCall(fname, args'))
    in 

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