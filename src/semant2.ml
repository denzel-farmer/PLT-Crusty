(* Semantic checking for the Crusty compiler *)

open Ast
open Sast
open Astprint

module StringMap = Map.Make(String)

let check program =
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
      match t with      
        | Prim (q, p) -> q, p
        | _ -> raise(Failure "")
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
            (* let err = "illegal binary operator " ^
                      string_of_typ t1 ^ " " ^ string_of_op ^ " " ^
                      string_of_typ t2 ^ " in " ^ string_of_expr e in *)
            let (q1, t1') = match_primitive t1 (* is this just extracting the type? *)
            and (q2, t2') = match_primitive t2
            (* All binary operators require operands of the same type*)
            in if t1 = t2 then
              (* Determine expression type based on operator and operand types *)
              let t = match op with
                  Add | Sub | Mul | Div when t1' = Int -> Int
                | Add | Sub | Mul | Div when t1' = Float -> Float
                | _ -> raise (Failure "")
              in
              (t1, SOperation(SArithOp((t1, e1'), op, (t2, e2'))))
            else raise (Failure "")
        | UnArithOp (op, e) -> 
            let (t, e') = check_expr e in 
            let err = "illegal unary operation " ^ string_of_typ t in 
            let q, t' = match_primitive t in
            let t' = match op with 
              Neg when t' = Int -> Int
              | Neg when t' = Bool -> Bool 
              | PreInc | PreDec | PostInc | PostDec when t' = Int -> Int
              | _ -> raise (Failure err)
            in  
            (t, SOperation(SUnArithOp(op, (t, e'))))
        | CompOp (e1, op, e2) -> 
          let (t1, e1') = check_expr e1
          and (t2, e2') = check_expr e2 in
          (* let err = "illegal compare operator " ^
                    string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                    string_of_typ t2 ^ " in " ^ string_of_expr e in *)
          (* TODO: allow compare operator to work for non-primitives ? *)
          let q1, t1' = match_primitive t1 in
          let q2, t2' = match_primitive t2 in 
          (* All compare operators require operands of the same type*)
          if t1' = t2' then
            let t = match op with
              | Eq | Neq | Lt | Gt | Leq | Geq when t1' = Int -> Bool
              | Eq | Neq | Lt | Gt | Leq | Geq when t1' = Float -> Bool
              | _ -> raise (Failure "")
            in
            (Prim(q1, t), SOperation(SCompOp((t1, e1'), op, (t2, e2'))))
          else raise (Failure "")
        | LogOp (e1, op, e2) -> 
          let (t1, e1') = check_expr e1
          and (t2, e2') = check_expr e2 in
          (* let err = "illegal binary logical operator " ^
                    string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                    string_of_typ t2 ^ " in " ^ string_of_expr e in *)
          let q1, t1' = match_primitive t1 in 
          let q2, t2' = match_primitive t2 in 
          if t1' = t2' then
            let t = match op with
                And | Or when t1' = Bool -> Bool
              | _ -> raise (Failure "")
            in
            (t1, SOperation(SLogOp((t1, e1'), op, (t2, e2'))))
          else raise (Failure "")
        | UnLogOp (op, e) -> 
          let (t, e') = check_expr e in 
          let err = "illegal unary logical operator " ^
                    string_of_typ t
          in 
          let q, t' = match t with 
            | Prim (q, p) when p = Bool -> q, p
            | _ -> raise (Failure err)
          (* if t != Bool then raise (Failure err) *)
          in
          (t, SOperation(SUnLogOp(op, (t, e'))))
        | AccessOp (e, op, var) -> 
          let (t, e') = check_expr e in
          let err = "illegal access operator " ^ string_of_typ t in     
          (* let t' = match_struct t (* fix ast definition? struct type is just a struct not stuct of string? *)
          in *)
          (t, SOperation(SAccessOp((t, e'), op, var)))
        | Deref (s) -> 
          let err = "illegal dereference operator " ^ s in
          let type_of_identifier t =
            try StringMap.find s symbols
            with Not_found -> raise (Failure ("undeclared identifier " ^ s))
          in 
          let t = type_of_identifier s in
          (t, SOperation(SDeref(s)))
        | Borrow (s) -> 
          let type_of_identifier t =
            try StringMap.find s symbols
            with Not_found -> raise (Failure ("undeclared identifier " ^ s))
          in 
          let t = type_of_identifier s in
          (t, SOperation(SBorrow(s)))
        | Index (s, e) -> 
          let (t, e') = check_expr e in
          let type_of_identifier t = 
            try StringMap.find s symbols 
            with Not_found -> raise (Failure ("undeclared identifier " ^ s)) in 
          (* let t, i = match_array t in  *)
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
        (Prim(Unrestricted, Int), SCall(fname, args'))
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