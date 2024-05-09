(* Semantic checking for the Crusty compiler *)

open Ast
open Sast
open Astprint

module StringMap = Map.Make (String)

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
        | _ when StringMap.mem field_name map -> make_err dup_err
        | _ ->  StringMap.add field_name field map
      in
      List.fold_left add_struct_field StringMap.empty st.fields;
      StringMap.add n st map;
  in 
  List.fold_left add_struct StringMap.empty program.structs;
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

    let match_primitive t = 
      let t = match t with      
        | Prim (q, p) -> q, p
        | _ -> raise(Failure "match_primitive")
      in t
    in 
    let match_struct t =
      let t = match t with 
        | Struct s -> s
        | _ -> raise(Failure "match_struct")
      in t
    in
    let match_array t = 
      let t = match t with 
      | Arr (t, i) -> t, i
      | _ -> raise (Failure "match_array")
      in t
    in 
    (* Check literals*)
    let check_literal = function 
        IntLit l -> (Prim(Unrestricted, Int), SLiteral(SIntLit(l)))
      | BoolLit l -> (Prim(Unrestricted, Bool), SLiteral(SBoolLit(l)))
      | CharLit l -> (Prim(Unrestricted, Char), SLiteral(SCharLit(l)))
      | FloatLit l -> (Prim(Unrestricted, Float), SLiteral(SFloatLit(l)))
      (* idk how to do this | StructLit l -> (Struct(), SStructLit l)  *)
      | StringLit l -> (Prim(Unrestricted, String), SLiteral(SStringLit(l)))
      (* idk how to do this | ArrayLit l -> ( , SArrayLit l) *)
    (* Return a semantically-checked expression, i.e., with a type *)
    in 
    let rec check_expr = function
      | Id var -> (type_of_identifier var, SId var)
      | Literal l -> check_literal l
      | Assignment l -> 
        let a = match l with 
          | Assign (var, e) -> 
            let lt = type_of_identifier var
            and (rt, e') = check_expr e in
            let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                      string_of_typ rt ^ " in " ^ string_of_expr e
            in
            (check_assign lt rt err, SAssignment(SAssign(var, (rt, e'))))
          (* | StructAssign (var1, var2, e) -> ()
          | RefStructAssign (var1, var2, e) -> ()
          | StructExplode (var, e) -> () *)
        in a
      | Operation l -> 
        let o = match l with 
          | ArithOp (e1, op, e2) -> 
            let (t1, e1') = check_expr e1
            and (t2, e2') = check_expr e2 in
            let err = "illegal binary operator " ^
                      string_of_typ t1 ^ " " ^ string_of_operation l ^ " " ^
                      string_of_typ t2 ^ " in " ^ string_of_expr e1 ^ string_of_expr e2 in
            let (q1, t1') = match_primitive t1 in (* is this just extracting the type? *)
            (* and (_, t2') = match_primitive t2 *)
            (* All binary operators require operands of the same type*)
            if t1 = t2 then
              (* Determine expression type based on operator and operand types *)
              let _ = match op with
                  Add | Sub | Mul | Div when t1' = Int -> Int
                | Add | Sub | Mul | Div when t1' = Float -> Float
                | _ -> raise (Failure err)
              in
              (t1, SOperation(SArithOp((t1, e1'), op, (t2, e2'))))
            else raise (Failure err)
        | UnArithOp (op, e) -> 
            let (t, e') = check_expr e in 
            let err = "illegal unary operation " ^ string_of_typ t in 
            let q, t' = match_primitive t in
            let _ = match op with 
              Neg when t' = Int -> Int
              | Neg when t' = Bool -> Bool 
              | PreInc | PreDec | PostInc | PostDec when t' = Int -> Int
              | _ -> raise (Failure err)
            in  
            (t, SOperation(SUnArithOp(op, (t, e'))))
        | CompOp (e1, op, e2) -> 
          let (t1, e1') = check_expr e1
          and (t2, e2') = check_expr e2 in
          let err = "illegal compare operator " ^
                    string_of_typ t1 ^ " " ^ string_of_operation l ^ " " ^
                    string_of_typ t2 ^ " in " ^ string_of_expr e1 ^ string_of_expr e2 in
          (* TODO: allow compare operator to work for non-primitives ? *)
          let q1, t1' = match_primitive t1 in
          let q2, t2' = match_primitive t2 in 
          (* All compare operators require operands of the same type*)
          if t1' = t2' then
            let t = match op with
              | Eq | Neq | Lt | Gt | Leq | Geq when t1' = Int -> Bool
              | Eq | Neq | Lt | Gt | Leq | Geq when t1' = Float -> Bool
              | _ -> raise (Failure err)
            in
            (Prim(q1, t), SOperation(SCompOp((t1, e1'), op, (t2, e2'))))
          else raise (Failure err)
        | LogOp (e1, op, e2) -> 
          let (t1, e1') = check_expr e1
          and (t2, e2') = check_expr e2 in
          let err = "illegal binary logical operator " ^
                    string_of_typ t1 ^ " " ^ string_of_operation l ^ " " ^
                    string_of_typ t2 ^ " in " ^ string_of_expr e1 ^ string_of_expr e2 in
          let q1, t1' = match_primitive t1 in 
          let q2, t2' = match_primitive t2 in 
          if t1' = t2' then
            let _ = match op with
                And | Or when t1' = Bool -> Bool
              | _ -> raise (Failure err)
            in
            (t1, SOperation(SLogOp((t1, e1'), op, (t2, e2'))))
          else raise (Failure err)
        | UnLogOp (op, e) -> 
          let (t, e') = check_expr e in 
          let err = "illegal unary logical operator " ^
                    string_of_typ t
          in  
          let _, _ = match t with 
            | Prim (q, p) when p = Bool -> q, p
            | _ -> raise (Failure err)
          (* if t != Bool then raise (Failure err) *)
          in
          (t, SOperation(SUnLogOp(op, (t, e'))))
        | AccessOp (e, op, var) -> 
          let (t, e') = check_expr e in
          let err = "illegal access operator " ^ string_of_typ t in     
          let _ = match_struct t (* fix ast definition? struct type is just a struct not stuct of string? *)
          in
          (t, SOperation(SAccessOp((t, e'), op, var)))
        | Deref (s) -> 
          let err = "illegal dereference operator " in
          let type_of_identifier t =
            try StringMap.find s symbols
            with Not_found -> raise (Failure (err))
          in 
          let t = type_of_identifier s in
          let err = "illegal dereference operator " ^ string_of_typ t in
          let t' = match t with 
            | Ref s -> s
            | _ -> raise (Failure(err))
          in 
          (t', SOperation(SDeref(s)))
        | Borrow (s) -> 
          let type_of_identifier t =
            try StringMap.find s symbols
            with Not_found -> raise (Failure ("undeclared identifier " ^ s))
          in 
          let t = type_of_identifier s in
          (Ref(t), SOperation(SBorrow(s)))
        | Index (s, e) -> 
          let err = "invalid access operation" in
          let (t, e') = check_expr e in
          let type_of_identifier t = 
            try StringMap.find s symbols 
            with Not_found -> raise (Failure ("undeclared identifier " ^ s)) in 
          let _, t' = match_primitive t in  
          if t' != Int then raise (Failure(err))
          else 
            let s' = type_of_identifier s in
            let arr_typ = match s' with 
              | Arr (t, s) -> t
              | _ -> raise(Failure(err))
            in  
            (t, SOperation(SIndex(s, (t, e'))))
        in o
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
        let args' = List.map2 check_call fd.args args in
        (* TODO: Fix this unrestricted int primitive being returned *)
        (Prim(Unrestricted, Int), SCall(fname, args'))
    in
    let check_bool_expr e =
      let (t, e') = check_expr e in
      let t = match t with
        | Prim(Unrestricted, Bool) -> (t, e')
        |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
      in t
    in 
    let rec check_stmt_list l = 
    match l with 
    [] -> []
    | Block sl :: sl' -> check_stmt_list (sl @ sl')
    | s :: sl -> check_stmt s :: check_stmt_list sl
    (* return a statement *)
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
    and check_return_stmt s = 
      let r = match s with 
        | Return e ->
          let (t, e') = check_expr e in
          let s = match func.rtyp with 
          | Nonvoid t -> t
          | _ -> raise (Failure ("Missing return statement"))
          in 
          if t = s then SReturn (t, e')
          else raise (Failure ("return gives "  ^ string_of_typ t))
          (* else raise (
              Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                        string_of_typ func.rtyp ^ " in " ^ string_of_expr e))  *)
        | VoidReturn -> 
          let s = match func.rtyp with 
          | Void -> SVoidReturn 
          | _ -> raise (Failure ("Missing return statement"))
          in s
      in r
    in 
    { 
      srtyp=func.rtyp;
      sfname=func.fname;
      sargs=func.args;
      slocals=func.locals;
      sbody=check_stmt_list func.body;
      sreturn=check_return_stmt func.return
    }
in
(program.globals, program.structs, List.map check_fun program.funcs)