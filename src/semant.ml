(* Semantic checking for the Crusty compiler *)

open Ast
open Sast
open Astprint

module StringMap = Map.Make (String)

(* Types for storing all struct definitions and members by name, could make this 
into separate module and share with the linear checker/parser  *)


type field_map = var_decl StringMap.t

(* TODO inefficient, stores fields as both list and map *)
type structs_map = (struct_def*field_map) StringMap.t


(* Compares two types, not considering linearity *)
let rec compare_stripped_types (first : typ) (second : typ) : bool =
  match first, second with
  | Prim(_, t1), Prim(_, t2) -> t1 = t2
  | Ref(t1), Ref(t2) -> compare_stripped_types t1 t2
  | Struct(s1), Struct(s2) -> s1 = s2
  | Arr(t1, i1), Arr(t2, i2) -> compare_stripped_types t1 t2 && i1 = i2
  | _ -> false 
;;

let rec compare_sexpr_decl_stripped_types (first : var_decl) (second : sexpr) : bool =
  let (t1, _) = first in
  let (t2, _) = second in
  compare_stripped_types t1 t2

let check program =
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

  (*TODO replace 'raise' with result/option *)
let gen_struct_map (structs : struct_def list) : structs_map =
  let gen_field_map (fields : var_decl list) : field_map =
    let add_field (map : field_map) (field : var_decl) =
      let (field_type, field_name) = field in
      if (StringMap.mem field_name map) then
        raise (Failure ("duplicate field name: " ^ field_name))
      else
        StringMap.add field_name field map
    in
    List.fold_left add_field StringMap.empty fields
  in
  let add_struct (map : structs_map) (st : struct_def) : structs_map =
    if (StringMap.mem st.sname map) then
      raise (Failure ("duplicate struct name: " ^ st.sname))
    else
      let fields = gen_field_map st.fields in
      StringMap.add st.sname (st, fields) map
  in
  List.fold_left add_struct StringMap.empty structs
in 
let struct_map = gen_struct_map program.structs in

  (* looks up a struct name (point) *)
  let find_struct (sname : string) = 
    match (StringMap.find_opt sname struct_map) with
    | None -> raise (Failure ("unrecognized struct " ^ sname))
    | Some s -> s
  in
  
  let scopes = ref [] in
  let enter_scope () = scopes := StringMap.empty :: !scopes in
  let exit_scope () = match !scopes with
  | [] -> raise (Failure "No scope to exit")
  | _ :: tl -> scopes := tl
  in

  let add_symbol (ty, name) =
    let current_map = List.hd !scopes in
    if StringMap.mem name current_map
    then raise (Failure ("duplicate variable " ^ name))
    else let updated_map = StringMap.add name ty current_map in
    scopes := updated_map :: (List.tl !scopes)
  in

  (* Return a variable from our local symbol table *)
  let type_of_identifier s =
    let rec lookup in_scopes =
      match in_scopes with
      | [] -> raise (Failure ("undeclared identifier " ^ s))
      | hd :: tl ->
          try StringMap.find s hd
          with Not_found -> lookup tl
    in lookup !scopes
  in

  enter_scope (); (* Enter global scope *)
  List.iter (fun (ty, name) -> add_symbol (ty, name)) program.globals;

  let check_fun func = 
    enter_scope (); (* Enter args/locals scope *)
    List.iter (fun (ty, name) -> add_symbol (ty, name)) func.args;
    List.iter (fun (ty, name) -> add_symbol (ty, name)) func.locals;

    (* Raise an exception if the given rvalue type cannot be assigned to
    the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if (compare_stripped_types lvaluet rvaluet) then lvaluet else raise (Failure err)
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
    (* Return a semantically-checked expression, i.e., with a type *)
   
    let rec check_expr (exp : expr) : sexpr =
        (* Check literals*)
      let check_literal (lit : literal): sexpr =
        match lit with 
          IntLit l -> (Prim(Unrestricted, Int), SLiteral(SIntLit(l)))
        | BoolLit l -> (Prim(Unrestricted, Bool), SLiteral(SBoolLit(l)))
        | CharLit l -> (Prim(Unrestricted, Char), SLiteral(SCharLit(l)))
        | FloatLit l -> (Prim(Unrestricted, Float), SLiteral(SFloatLit(l)))
        | StringLit l -> (Prim(Unrestricted, String), SLiteral(SStringLit(l)))
        | ArrayLit l -> raise (Failure "ArrayLit not implemented")
        | StructLit (name, exprs) -> 
          match (StringMap.find_opt name struct_map) with
          | None -> raise (Failure ("undeclared struct \"" ^ name ^ "\""))
          | Some (st, fields) ->
            let sexprs = List.map check_expr exprs in
            let type_match = List.for_all2 compare_sexpr_decl_stripped_types st.fields sexprs in
            if not type_match then
              raise (Failure ("type mismatch in struct literal \"" ^ name ^ "\""))
            else
            (Struct(name), SLiteral(SStructLit(name, sexprs)))
        in
      match exp with
      | Id var -> (type_of_identifier var, SId var)
      | Literal l -> check_literal l
      | Assignment l -> 
        let a = match l with 
          | Assign (e1, e2) -> 
            let (lt, e1') = check_expr e1
            and (rt, e2') = check_expr e2
            in
            let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                      string_of_typ rt ^ " in " ^ string_of_expr e1
            in
            (check_assign lt rt err, SAssignment(SAssign((lt, e1'), (rt, e2'))))
          | StructAssign (var1, var2, e) -> 
            (* Get struct definition from global struct map *)
            let stype = type_of_identifier var1 in 
            let get_sname = function 
              | Struct s -> s 
              | _ -> raise (Failure "Not a struct type")
            in 
            let s = find_struct (get_sname stype) in
            let (sdef, smap) = match s with 
              | (a, b) -> a, b 
            in 
            let (lt, _) = 
              try StringMap.find var2 smap 
              with Not_found -> raise (Failure ("Field "^ var2 ^" not found in struct " ^ var1))
            in
            let (rt, e') = check_expr e in
            let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                      string_of_typ rt ^ " in " ^ string_of_expr e
            in
            (check_assign lt rt err, SAssignment(SStructAssign(var1, var2, (rt, e'))))
          | RefStructAssign (var1, var2, e) -> 
            let stype = type_of_identifier var1 in 
            let get_sname = function 
              | Ref(Struct s) -> s 
            in 
            let s = find_struct (get_sname stype) in 
            let (sdef, smap) = match s with 
              | (a, b) -> a, b
            in
            let (lt, _) = 
            try StringMap.find var2 smap 
            with Not_found -> raise (Failure ("Field "^ var2 ^" not found in struct " ^ var1))
            in
            let (rt, e') = check_expr e in
            let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                      string_of_typ rt ^ " in " ^ string_of_expr e
            in
            (check_assign lt rt err, SAssignment(SRefStructAssign(var1, var2, (rt, e'))))
          | StructExplode (var, e) -> 
(* 
            let (t, e) = check_expr e in 
            (t, SStructExplode(var, (t, e)))
            ) *)

            let (struct_type, struct_expr) = check_expr e in
            match struct_type with
            | Struct sname ->
              let (struct_def, field_map) = find_struct sname in
              if List.length var <> List.length struct_def.fields then
                raise (Failure "Number of variables does not match number of structure fields")
              else
                let field_types = List.map (fun (t, _) -> t) struct_def.fields in
                let var_types = List.map type_of_identifier var in
                List.iter2 (fun expected found ->
                  if not (compare_stripped_types expected found) then
                    raise (Failure "Type mismatch in struct explode")
                ) field_types var_types;
                (struct_type, SAssignment(SStructExplode (var, (struct_type, struct_expr))))
            | _ -> raise (Failure "Right hand side of StructExplode is not a structure")
                    
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
            if compare_stripped_types t1 t2 then
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
          if compare_stripped_types t1 t2 then
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
          if compare_stripped_types t1 t2 then
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
        | AccessOp (s, op, var) -> 
          let stype = type_of_identifier s in
          let get_sname = function 
              | Struct s -> s 
              | _ -> raise (Failure "Not a struct type")
          in 
          let s' = find_struct (get_sname stype) in
          let (sdef, smap) = match s' with 
            | (a, b) -> a, b 
          in 
          let (t, _) = 
            try StringMap.find var smap 
            with Not_found -> raise (Failure ("Field "^ var ^" not found in struct " ^ s))
          in
          (t, SOperation(SAccessOp((s, op, var))))
        | Deref (s) -> 
          let err = "illegal dereference operator " in
          let t = type_of_identifier s in
          let err = "illegal dereference operator " ^ string_of_typ t in
          let t' = match t with 
            | Ref s -> s
            | _ -> raise (Failure(err))
          in 
          (t', SOperation(SDeref(s)))
        | Borrow (s) -> 
          let t = type_of_identifier s in
          (Ref(t), SOperation(SBorrow(s)))
        | Index (s, e) -> 
          let err = "invalid access operation" in
          let (t, e') = check_expr e in
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
    (* | Block sl :: sl' -> check_stmt_list (sl @ sl') *)
    | s :: sl -> check_stmt s :: check_stmt_list sl
    (* return a statement *)
    and check_stmt s = 
      match s with 
        Block (vl, sl) -> 
          enter_scope ();
          List.iter (fun (ty, name) -> add_symbol (ty, name)) vl;
          let checked_stmts = check_stmt_list sl in
          exit_scope ();
          SBlock (vl, checked_stmts)
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
          if compare_stripped_types t s then SReturn (t, e')
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
    let checked_body = check_stmt_list func.body in
    let checked_return = check_return_stmt func.return in 
    exit_scope (); (* Exit args/locals scope *)
    { 
      srtyp=func.rtyp;
      sfname=func.fname;
      sargs=func.args;
      slocals=func.locals;
      sbody=checked_body;
      sreturn=checked_return
    }
in
(program.globals, program.structs, List.map check_fun program.funcs)