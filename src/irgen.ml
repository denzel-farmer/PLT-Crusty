module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, structs, functions) =
  let context    = L.global_context () in
  let the_module = L.create_module context "Crusty" in

  let i64_t      = L.i64_type       context
  and i32_t      = L.i32_type       context
  and i8_t       = L.i8_type        context
  and i1_t       = L.i1_type        context 
  (* and iString_t  = L.pointer_type (L.i8_type context) *)
  in
  
  let struct_decls : (A.struct_def) StringMap.t =
    let struct_decl m (sdecl: A.struct_def) =
      let name = sdecl.sname in
      StringMap.add name (sdecl) m in
    List.fold_left struct_decl StringMap.empty structs
  in

  (* let rec struct_fields s = 
    let struct_decl = StringMap.find s struct_decls in
    let field_types = List.map (fun (t, _) -> ltype_of_typ t) struct_decl.fields in
    Array.of_list field_types
  in *)

  let rec ltype_of_typ = function
    A.Prim(_, Int) -> i32_t
  | A.Prim(_, Char) -> i8_t
  | A.Prim(_, Bool) -> i1_t
  | A.Prim(_, Float) -> L.float_type context
  | A.Prim(_, String) -> L.pointer_type i8_t
  | A.Struct(s) ->
    let struct_decl = StringMap.find s struct_decls in
    let field_types = List.map (fun (t, _) -> ltype_of_typ t) struct_decl.fields in
    L.struct_type context (Array.of_list field_types)
  | A.Arr(t, n) -> L.array_type (ltype_of_typ t) n 
  (* | A.Ref(t) ->  *) (* TODO *)
  in 

  let rec ltype_of_rtyp = function 
    A.Nonvoid(t) -> ltype_of_typ t
    | A.Void -> L.void_type context
  in
  
  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sargs)
      in 
      let ftype = L.function_type (ltype_of_rtyp fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m 
    in
    List.fold_left function_decl StringMap.empty functions
  in
    
  let build_literal lit builder =
    match lit with
    | SIntLit l -> L.const_int i32_t l
    | SBoolLit l -> L.const_int i1_t (if l then 1 else 0)
    | SCharLit l -> L.const_int i8_t (Char.code l)
    | SFloatLit l -> L.const_float (L.float_type context) l
    | SStringLit s -> 
      let str = L.build_global_stringptr s "globalstring" builder in
      L.const_bitcast str (L.pointer_type i8_t) 
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = match t with
        A.Prim (_, _) -> L.const_int (ltype_of_typ t) 0
      (* | A.Arr(_) -> ltype_of_typ t *)
      in StringMap.add n (L.define_global n init the_module) m 
    in
    List.fold_left global_var StringMap.empty globals
  in
  
  (* let struct_defs : L.llvalue StringMap.t =
    let struct_def m l =
      let init = ltype_of_typ l
      in StringMap.add l.sname (L.define_global n init the_module) m 
    in
    List.fold_left struct_def StringMap.empty structs 
  in *)

  (* Define the printf function *)

  (* let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in *)

  let scopes = ref [] in

  let enter_scope () = scopes := StringMap.empty :: !scopes in
  
  let exit_scope () = match !scopes with
    | [] -> raise (Failure "No scope to exit")
    | _ :: tl -> scopes := tl
  in
  
  let add_local builder var_name lltype =
    let current_map = List.hd !scopes in
    let alloca = L.build_alloca lltype var_name builder in
    scopes := (StringMap.add var_name alloca current_map) :: (List.tl !scopes);
    alloca
  in
  
  let find_local var_name =
    let rec lookup scopes = match scopes with
      | [] -> raise (Failure ("Unbound variable " ^ var_name))
      | h :: t -> (try StringMap.find var_name h with Not_found -> lookup t)
    in lookup !scopes   
  in

  let build_function_body fdecl =
    let (function_addr, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block function_addr) in

    (* let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in *)

    enter_scope ();

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    (* Handle function parameters *)
    let params = Array.to_list (L.params function_addr) in
    List.iteri (fun i (ty, n) ->
        let param_type = ltype_of_typ ty in
        let param = List.nth params i in
        let _ = add_local builder n param_type in
        ignore (L.build_store param (find_local n) builder)
    ) fdecl.sargs;
    
    (* Handle local variables *)
    (* List.iter (fun (ty, n) ->
      ignore (add_local builder n (ltype_of_typ ty))
    ) fdecl.slocals; *)

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try find_local n
      with Failure _ -> StringMap.find n global_vars
    in
    let rec build_expr builder ((_, e) : sexpr) = match e with
        | SId s -> L.build_load (lookup s) s builder
        | SLiteral l  -> build_literal l builder
        | SAssignment sa -> 
          (match sa with 
          | SAssign (s, e) ->
            let e' = build_expr builder e in
            ignore(L.build_store e' (lookup s) builder); e'
          | _ -> raise (Failure "Invalid assignment"))
        | SOperation so ->
          (match so with 
          | SArithOp (e1, op, e2) -> 
              let e1' = build_expr builder e1 
              and e2' = build_expr builder e2 
              and (ty, sx) = e1 in 
              (match op with 
                  | A.Add -> (match ty with 
                      | A.Prim(_, Int) -> L.build_add
                      | A.Prim(_, Float) -> L.build_fadd)
                  | A.Sub -> (match ty with 
                      | A.Prim(_, Int) -> L.build_sub
                      | A.Prim(_, Float) -> L.build_fsub)
                  | A.Mul -> (match ty with
                      | A.Prim(_, Int) -> L.build_mul
                      | A.Prim(_, Float) -> L.build_fmul)
                  | A.Div -> (match ty with
                      | A.Prim(_, Int) -> L.build_sdiv
                      | A.Prim(_, Float) -> L.build_fdiv)
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
          | _ -> raise (Failure "Not implemented: SAccessOp, SDerefOp, SBorrow, SIndex")
          )
          (* | SCall ("print", [e]) ->
            L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
              "printf" builder *)
          | SCall (f, args) ->
            let (fdef, fdecl) = StringMap.find f function_decls in
            let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
            let result = f ^ "_result" in
            L.build_call fdef (Array.of_list llargs) result builder  
        
    in

     (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
      let add_terminal builder instr =
        match L.block_terminator (L.insertion_block builder) with
          Some _ -> ()
        | None -> ignore (instr builder)
      in

    let rec build_stmt builder = function
        SBlock (vl, sl) -> 
          enter_scope ();

          List.iter (fun (ty, name) ->
            ignore (add_local builder name (ltype_of_typ ty))
          ) vl;
          
          let result_builder = List.fold_left build_stmt builder sl in

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
      (* | SBreak -> 
      | SContinue -> ignore(L.build_br while_bb builder) *)
    in
    (* Build the code for each statement in the function *)
    let func_builder = build_stmt builder (SBlock (fdecl.slocals, fdecl.sbody)) 
    in
    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0));
    
    exit_scope ();
  in
List.iter build_function_body functions; 
the_module