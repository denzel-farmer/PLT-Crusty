(* Semantic checking for the MicroC compiler *)

open Ast
open Sast
open Astprint

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

  (* TODO: Add checking for structs to make sure no duplicate variables *)
  let check_struct struct = 
    (* struct.name *)
    (* struct.fields *)

  in 

  let check_fun func = 
    check_binds "arg" func.args;
    check_binds "local" func.locals;

    (* Raise an exception if the given rvalue type cannot be assigned to
    the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    let add_symbol m (ty, name) = StringMap.add name ty m 
    in 
    let symbols = List.fold_left add_symbol StringMap.empty (globals @ func.args @ func.locals)
    in 

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in 

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr = function
      Id var -> (type_of_identifier var, SId var)
      | Literal l -> check_literal l 
      | Assign l -> check_all_assignment l
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
    (* Check literals*)
    let check_literal = function 
        IntLit l -> (Int, SIntLit l)
      | BoolLit l -> (Bool, SBoolLit l)
      | CharLit l -> (Char, SCharLit l)
      | FloatLit l -> (Float, SFloatLit l)
      (* | StructLit l -> (Struct, ) *)
      | StringLit l -> (String, SStringLit l)
      (* | ArrayLit l -> () *)
    in 
    (* Check assignments*) 
    let check_all_assignment = function 
        Assign (var, e) -> ()
      | StructAssign (var1, var2, e) -> ()
      | RefStructAssign (var1, var2, e) -> ()
      (* | StructExplode (var, e) *)
    in 
    (* Check operations *)
    let check_operation = function 
        ArithOp (e1, op, e2) -> 
        let (t1, e1') = check_expr e1
        and (t2, e2') = check_expr e2 in
        let err = "illegal binary operator " ^
                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e
        in
        (* All binary operators require operands of the same type*)
        if t1 = t2 then
          (* Determine expression type based on operator and operand types *)
          let t = match op with
              Add | Sub | Mul | Div when t1 = Int -> Int
              Add | Sub | Mul | Div when t1 = Float -> Float
            | _ -> raise (Failure err)
          in
          (t, SArithop((t1, e1'), op, (t2, e2')))
        else raise (Failure err)
      | UnArithOp (op, e) -> 
        let (t, e) = check_expr e1 in 
        let err = "illegal unary operation " ^ string_of_typ t 
        in  
        let t = match op with 
           
        in 
        (t, SUnArithop(op, (t, e))
      | CompOp (e1, op, e2) -> 
        let (t1, e1') = check_expr e1
        and (t2, e2') = check_expr e2 in
        let err = "illegal compare operator " ^
                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e
        in
        (* All compare operators require operands of the same type*)
        if t1 = t2 then
          let t = match op with
              Eq | Neq -> Bool 
            | Lt | Gt | Leq | Geq when t1 = Int -> Bool 
            | Lt | Gt | Leq | Geq when t1 = Float -> Bool 
            | _ -> raise (Failure err)
          in
          (t, SCompOp((t1, e1'), op, (t2, e2')))
        else raise (Failure err)
      | LogOp (e1, op, e2) -> 
        let (t1, e1') = check_expr e1
        and (t2, e2') = check_expr e2 in
        let err = "illegal binary logical operator " ^
                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e
        in
        (* Both must be bools *)
        if t1 = t2 then
          let t = match op with
              And | Or when t1 = Bool -> Bool
            | _ -> raise (Failure err)
          in
          (t, SLogOp((t1, e1'), op, (t2, e2')))
        else raise (Failure err)
      | UnLogOp (op, e) -> 
        let (t, e') = check_expr e in 
        let err = "illegal unary logical operator " ^
                  string_of_typ t
        in 
        if t != Bool -> raise (Failure err)
        in
        (t, SUnLogOp(op, (t, e')))
      | AccessOp (e, op, var) -> 
        let (t, e') = check_expr e in
        let err = "illegal access operator " ^
        string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
        string_of_typ t2 ^ " in " ^ string_of_expr e
in 
        if t != Struct 

      | Deref (e) -> () 
      | Borrow (e) -> ()
    in 

      
    let check_bool_expr e =
      let (t, e') = check_expr e in
      match t with
      | Bool -> (t, e')
      |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
    in

    let rec check_stmt_list l = 
    match l with 
    [] -> [] 
    | s :: sl -> check_stmt s :: check_stmt_list sl
    and check_stmt s = 
      match s with 
      Block sl -> SBlock (check_stmt_list sl)
      | Expr e -> SExpr (check_expr e)
      | If(e, st1, st2) ->
        SIf(check_bool_expr e, check_stmt st1, check_stmt st2)
      | While(e, st) ->
        SWhile(check_bool_expr e, check_stmt st)
      | Break -> SBreak
      | Continue -> SContinue
      | Return e ->
        let (t, e') = check_expr e in
        if t = func.rtyp then SReturn (t, e')
        else raise (
            Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                      string_of_typ func.rtyp ^ " in " ^ string_of_expr e)) 
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