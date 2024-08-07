module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate ((globals : A.var_decl list), (structs : A.struct_def list), (functions : sfunc_def list)) =
  let context    = L.global_context () in
  let crusty_module = L.create_module context "Crusty" in

  (* LLVM integer types *)
  let i32_t      = L.i32_type       context
  and i8_t       = L.i8_type        context
  and i1_t       = L.i1_type        context 
  in
  
  (* Map of struct name to struct definition *)
  let struct_decls : (A.struct_def) StringMap.t =
    let add_struct_def m (sdef: A.struct_def) =
      let name = sdef.sname in
      StringMap.add name (sdef) m in
    List.fold_left add_struct_def StringMap.empty structs
  in

  (* Convert AST type to LLVM type *)
  let rec ltype_of_typ : (A.typ -> L.lltype) = function
      A.Prim(_, Int) -> i32_t
    | A.Prim(_, Char) -> i8_t
    | A.Prim(_, Bool) -> i1_t
    | A.Prim(_, Float) -> L.float_type context
    | A.Prim(_, String) -> L.pointer_type i8_t
    | A.Struct(s) ->
      let struct_def = StringMap.find s struct_decls in
      let field_types = List.map (fun (t, _) -> ltype_of_typ t) struct_def.fields in
      L.struct_type context (Array.of_list field_types)
    | A.Ref(t) -> L.pointer_type (ltype_of_typ t)
    | A.Arr(t, n) -> raise (Failure "Not implemented: Array")
  in 

  let rec ltype_of_rtyp : (A.ret_typ -> L.lltype) = function 
      A.Nonvoid(t) -> ltype_of_typ t
    | A.Void -> L.void_type context
  in

  (* Build LLVM literal from Crusty literal *)
  let build_literal lit builder : L.llvalue =
    match lit with
    | SIntLit l -> L.const_int i32_t l
    | SBoolLit l -> L.const_int i1_t (if l then 1 else 0)
    | SCharLit l -> L.const_int i8_t (Char.code l)
    | SFloatLit l -> L.const_float (L.float_type context) l
    | SStringLit s -> L.build_global_stringptr s "globalstring" builder
    | _ -> raise (Failure "Not implemented: StructLit")
  in

  (* Map of function name to (LLVM type, function definition) *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let add_function_decl m fdecl =
      let name = fdecl.sfname
      and args_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sargs)
      in 
      let ftype = L.function_type (ltype_of_rtyp fdecl.srtyp) args_types in
      StringMap.add name (L.define_function name ftype crusty_module, fdecl) m 
    in
    List.fold_left add_function_decl StringMap.empty functions
  in

  (* Scoped symbol table, list of maps of variable names to (address, type) *)
  let symbols = ref [] in

  let enter_scope () = symbols := StringMap.empty :: !symbols in
  
  let exit_scope () = match !symbols with
    | [] -> raise (Failure "No scope to exit")
    | _ :: tl -> symbols := tl
  in

  (* Add globals to symbol table *)
  let global_vars : (L.llvalue * A.typ) StringMap.t =
    let add_global_var m (t, n) =
      let init = match t with
        A.Prim (_, _) -> L.const_int (ltype_of_typ t) 0
        | _ -> raise (Failure "Global variable not a primitive type")
      in 
      StringMap.add n (L.define_global n init crusty_module, t) m 
    in
    List.fold_left add_global_var StringMap.empty globals
  in

  symbols := [global_vars];

  (* Helper function to add locals to symbol table *)
  let add_symbol builder var_name var_type lltype =
    let current_map = List.hd !symbols in
    let alloca = L.build_alloca lltype var_name builder in
    symbols := (StringMap.add var_name (alloca, var_type) current_map) :: (List.tl !symbols);
    alloca
  in
  
  (* Return the value for a variable or formal argument.
      Check local names first, then global names *)
  let get_symbol var_name =
    let rec search (symbols) = match symbols with
      | [] -> raise (Failure ("Unbound variable " ^ var_name))
      | h :: t -> 
        try StringMap.find var_name h with Not_found -> search t
    in search !symbols
  in

  (* function to find the offset of a given field in a struct *)
  let get_field_idx (all_fields : A.var_decl list) (field : string) = 
    let rec find_field fields field num = 
      match fields with 
      | [] -> raise (Failure ("Struct field not found: " ^ field)) 
      | hd :: tl ->
        let (_, field') = hd in
        if field' = field then num else find_field tl field (num + 1)
    in
    find_field all_fields field 0 
  in

  (* Build in print function *)
  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
  in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t crusty_module in


  (* Build each function body *)
  let build_function_body fdecl =
    let (function_addr, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block function_addr) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let char_format_str = L.build_global_stringptr "%c\n" "char_fmt" builder in
    let float_format_str = L.build_global_stringptr "%f\n" "float_fmt" builder in
    let string_format_str = L.build_global_stringptr "%s" "str_fmt" builder in

    (* Add args and locals to symbol table *)
    let params = Array.to_list (L.params function_addr) in
    List.iteri (fun i (ty, n) ->
      let param_type = ltype_of_typ ty in
      let param = List.nth params i in
      let _ = add_symbol builder n ty param_type in
      let a' = get_symbol n in
      let a = match a' with 
        (t, _) -> t
      in  
      ignore (L.build_store param a builder)
    ) fdecl.sargs;
    
    (* Build expressions *)
    let rec build_expr builder ((_, e) : sexpr) : L.llvalue = match e with
      | SId s -> let addr, _ = get_symbol s in
        L.build_load addr s builder
      | SLiteral l -> build_literal l builder
      | SAssignment s -> 
        (match s with 
        | SAssign (e1, e2) ->
          let e1' = match e1 with 
            | (_, SId(s)) -> let addr, _ = get_symbol s in addr
            | _ -> build_expr builder e1
          in
          let e2' = build_expr builder e2 in
          ignore(L.build_store e2' e1' builder); e2'
        | SDerefAssign (s, e) -> 
          let e' = build_expr builder e in
          let ptr_to_ptr, _ = get_symbol s in
          let actual_ptr = L.build_load ptr_to_ptr "deref" builder in
          L.build_store e' actual_ptr builder
        | SStructAssign (s1, s2, e) -> 
          let e' = build_expr builder e in
          let (struct', st) = get_symbol s1 in
          let s_decl = match st with 
            A.Struct(s) -> StringMap.find s struct_decls
            | _ -> raise (Failure "StructAssign: Not a struct")
          in 
          let index = get_field_idx s_decl.fields s2 in
          let field' = L.build_struct_gep struct' index "tmp" builder in
          L.build_store e' field' builder
        | SRefStructAssign (s1, s2, e) -> 
          let e' = build_expr builder e in
          let (s, st) = get_symbol s1 in
          let t' = match st with 
            A.Ref(Struct(s)) -> StringMap.find s struct_decls
            | _ -> raise (Failure "RefStructAssign: Not a struct")
          in 
          let index = get_field_idx t'.fields s2 in
          let struct' = L.build_load s "tmp" builder in
          let field' = L.build_struct_gep struct' index "tmp" builder in
          L.build_store e' field' builder
        | SStructExplode (var_names, e) -> 
          let (_, e_expr) = e in
          (match e_expr with
            | SId struct_name -> 
              let (struct_addr, struct_typ) = get_symbol struct_name in
              List.iteri (fun idx var_name ->
                let field_ptr = L.build_struct_gep struct_addr idx "field_ptr" builder in
                let field_val = L.build_load field_ptr "field_val" builder in
                let var_addr, _ = get_symbol var_name in
                ignore (L.build_store field_val var_addr builder)
              ) var_names; struct_addr
            |_ -> raise (Failure "StructExplode: Not a struct")
          )
        )
      | SOperation s ->
        (match s with 
        | SArithOp (e1, op, e2) -> 
          let e1' = build_expr builder e1 
          and e2' = build_expr builder e2 
          and (ty, sx) = e1 in 
          (match op with 
            | A.Add -> (match ty with 
              | A.Prim(_, Int) -> L.build_add
              | A.Prim(_, Float) -> L.build_fadd
              | _ -> raise (Failure "Add: Not a Int or Float"))
            | A.Sub -> (match ty with 
              | A.Prim(_, Int) -> L.build_sub
              | A.Prim(_, Float) -> L.build_fsub
              | _ -> raise (Failure "Sub: Not a Int or Float"))
            | A.Mul -> (match ty with
              | A.Prim(_, Int) -> L.build_mul
              | A.Prim(_, Float) -> L.build_fmul
              | _ -> raise (Failure "Mul: Not a Int or Float"))
            | A.Div -> (match ty with
              | A.Prim(_, Int) -> L.build_sdiv
              | A.Prim(_, Float) -> L.build_fdiv
              | _ -> raise (Failure "Div: Not a Int or Float"))
            | A.Mod -> raise (Failure "Not implemented: Mod")
          ) e1' e2' "tmp" builder 
        | SUnArithOp(op, e) -> 
          let e' = build_expr builder e in
          (match op with 
            | A.Neg -> L.build_neg
            | _ -> raise (Failure "Not implemented: PreInc, PreDec, PostInc, PostDec")
          ) e' "tmp" builder
        | SCompOp (e1, op, e2) ->
            let e1' = build_expr builder e1 
            and e2' = build_expr builder e2 in 
            (match op with 
              | A.Eq -> L.build_icmp L.Icmp.Eq
              | A.Neq -> L.build_icmp L.Icmp.Ne
              | A.Lt -> L.build_icmp L.Icmp.Slt
              | A.Gt -> L.build_icmp L.Icmp.Sgt
              | A.Leq -> L.build_icmp L.Icmp.Sle
              | A.Geq -> L.build_icmp L.Icmp.Sge
            ) e1' e2' "tmp" builder
        | SLogOp (e1, op, e2) -> 
          let e1' = build_expr builder e1 
          and e2' = build_expr builder e2 in 
          (match op with 
            | A.And -> L.build_and
            | A.Or -> L.build_or
          ) e1' e2' "tmp" builder
        | SUnLogOp (op, e) -> 
          let e' = build_expr builder e in
          (match op with 
            | A.Not -> L.build_not
          ) e' "tmp" builder
        | SAccessOp (s1, op, s2) -> 
          (match op with 
            | Arrow -> 
              let (struct', st) = get_symbol s1 in
              let struct_val = L.build_load struct' "tmp" builder in
              let s = match st with 
                A.Ref(Struct(s)) -> StringMap.find s struct_decls
                | _ -> raise (Failure "Arrow: Not a struct")
              in
              let index = get_field_idx s.fields s2 in
              let field' = L.build_struct_gep struct_val index s2 builder in
              L.build_load field' "tmp" builder
            | Dot -> 
              let (struct', st) = get_symbol s1 in
              let s_def = match st with 
                A.Struct(s) -> StringMap.find s struct_decls
                | _ -> raise (Failure "Dot: Not a struct")
              in 
              let index = get_field_idx s_def.fields s2 in
              let field' = L.build_struct_gep struct' index s2 builder in
              L.build_load field' "tmp" builder)
        | SDeref s -> 
          let addr, _ = get_symbol s in
          let v = L.build_load addr "tmp" builder in
          L.build_load v (s ^ "_val") builder 
        | SBorrow s -> 
          let (s_addr, s') = get_symbol s in s_addr
        )
      | SCall ("print", [e]) -> 
        let ty, _ = e in
        let e' = build_expr builder e in
        let format_str = (match ty with
          | A.Prim(_, Int) -> int_format_str
          | A.Prim(_, Char) -> char_format_str
          | A.Prim(_, Bool) -> int_format_str
          | A.Prim(_, Float) -> float_format_str
          | A.Prim(_, String) -> string_format_str
          | _ -> raise (Failure "Print: Not a primitive type"))
        in
        L.build_call printf_func [| format_str; e' |] "printf" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
        let result = f ^ "_result" in
        L.build_call fdef (Array.of_list llargs) result builder  
    in

    (* Ensure that each basic block end with exactly one "terminator" *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder)
    in

    (* Build statements *)
    let rec build_stmt builder = function
        SBlock (vl, sl) -> 
          enter_scope ();
          List.iter (fun (ty, name) ->
            ignore (add_symbol builder name ty (ltype_of_typ ty))
          ) vl;
          let result_builder = List.fold_left build_stmt builder sl in
          exit_scope ();
          result_builder
      | FSBlock (vl, sl, r) -> 
        enter_scope ();
          List.iter (fun (ty, name) ->
            ignore (add_symbol builder name ty (ltype_of_typ ty))
          ) vl;
          let result_builder = List.fold_left build_stmt builder sl in

          (* Function to handle return statements at the end of functions *)
          let handle_return_stmt builder =
            match r with
            | SVoidReturn -> ignore (L.build_ret_void builder)
            | SReturn expr -> 
              let return_val = build_expr builder expr in
              ignore (L.build_ret return_val builder)
          in
          handle_return_stmt result_builder;

          exit_scope ();
          result_builder
      | SExpr e -> ignore(build_expr builder e); builder
      | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = build_expr builder predicate in

        let then_bb = L.append_block context "then" function_addr in
        ignore (build_stmt (L.builder_at_end context then_bb) then_stmt);
        let else_bb = L.append_block context "else" function_addr in
        ignore (build_stmt (L.builder_at_end context else_bb) else_stmt);

        let end_bb = L.append_block context "if_end" function_addr in
        let build_br_end = L.build_br end_bb in (* partial function *)
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context end_bb

      | SWhile (predicate, body) ->
        let while_bb = L.append_block context "while" function_addr in
        let build_br_while = L.build_br while_bb in (* partial function *)
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr while_builder predicate in

        let body_bb = L.append_block context "while_body" function_addr in
        add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;

        let end_bb = L.append_block context "while_end" function_addr in

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        L.builder_at_end context end_bb
      | _ -> raise (Failure "Not implemented: SContinue, SBreak")
    in

    (* Build the code for each statement in the function *)
    let func_builder = build_stmt builder (FSBlock (fdecl.slocals, fdecl.sbody, fdecl.sreturn)) in

    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0));
  in

  List.iter build_function_body functions;
  crusty_module