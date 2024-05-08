module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, structs, functions) =
  let context    = L.global_context () in

  let the_module = L.create_module context "MicroC" in

  let i64_t      = L.i64_type    context
  and i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context in

  let ltype_of_typ = function
      A.Prim(Unrestricted, Int) -> i32_t
    | A.Prim(Restricted, Int) -> i32_t
    | A.Prim(Unrestricted, Char) -> i8_t
    | A.Prim(Restricted, Char) -> i8_t
    | A.Prim(Unrestricted, Bool) -> i1_t
    | A.Prim(Restricted, Bool) -> i1_t
    | A.Prim(Unrestricted, Float)  -> i64_t
    | A.Prim(Unrestricted, Float) -> i64_t
    | A.String -> 
  in 

  let lookup n = try StringMap.find n local_vars
      with Not_found -> StringMap.find n global_vars
    in

  let build_literal = function 
    | SIntLit l -> L.const_int i32_t l
    | SBoolLit l -> L.const_int i1_t l
    | SCharLit l -> L.const_int i8_t l
    | SFloatLit l -> L.const_int i64_t l

  let rec build_expr builder ((_, e) : sexpr) = match e with
      | SId s -> L.build_load (lookup s) s builder
      | SLiteral l  -> build_literal l
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SAssign (s, e) -> let e' = build_expr builder e in
        ignore(L.build_store e' (lookup s) builder); e'
      | SBinop (e1, op, e2) ->
        let e1' = build_expr builder e1
        and e2' = build_expr builder e2 in
        (match op with
           A.Add     -> L.build_add
         | A.Sub     -> L.build_sub
         | A.And     -> L.build_and
         | A.Or      -> L.build_or
         | A.Equal   -> L.build_icmp L.Icmp.Eq
         | A.Neq     -> L.build_icmp L.Icmp.Ne
         | A.Less    -> L.build_icmp L.Icmp.Slt
        ) e1' e2' "tmp" builder
      | SCall ("print", [e]) ->
        L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
          "printf" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
        let result = f ^ "_result" in
        L.build_call fdef (Array.of_list llargs) result builder
    in


  