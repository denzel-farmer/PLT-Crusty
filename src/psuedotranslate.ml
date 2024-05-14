open Ast
open Sast
open Log
module StringMap = Map.Make (String)

type field_info = var_decl StringMap.t
type struct_info = (struct_def * field_info) StringMap.t

(* Helper module for managing structures *)
(* TODO integrate this into linear.ml and semant.ml *)

let string_of_struct_info (struct_info : struct_info) : string =
  if StringMap.is_empty struct_info
  then "[]"
  else
    StringMap.fold
      (fun key (sdef, field_info) acc ->
        acc ^ key ^ " -> " ^ Astprint.string_of_struct_def sdef ^ "\n")
      struct_info
      ""
;;

(* Take in a list of structure definitions and compute a struct_info object *)
let gen_struct_info (structs : struct_def list) : struct_info =
  let gen_field_map (fields : var_decl list) : field_info =
    let add_field (map : field_info) (field : var_decl) =
      let field_type, field_name = field in
      if StringMap.mem field_name map
      then raise (Failure ("duplicate field name: " ^ field_name))
      else StringMap.add field_name field map
    in
    List.fold_left add_field StringMap.empty fields
  in
  let add_struct (map : struct_info) (st : struct_def) : struct_info =
    if StringMap.mem st.sname map
    then raise (Failure ("duplicate struct name: " ^ st.sname))
    else (
      let fields = gen_field_map st.fields in
      StringMap.add st.sname (st, fields) map)
  in
  info_println "Generating struct info";
  let struct_info = List.fold_left add_struct StringMap.empty structs in
  debug_println ("Struct info: " ^ string_of_struct_info struct_info);
  struct_info
;;

(* Given a struct name and struct_info object, retrieve the struct definition*)
let get_struct_def (struct_name : string) (struct_info : struct_info) : struct_def option =
  let opt_struct = StringMap.find_opt struct_name struct_info in
  match opt_struct with
  | Some (sdef, _) -> Some sdef
  | None -> None
;;

(* Given a struct name and the index of a field, retrieves the member name of that field *)
let get_field (struct_name : string) (field_index : int) (struct_info : struct_info)
  : var_decl
  =
  let sdef = get_struct_def struct_name struct_info in
  match sdef with
  | Some sdef ->
    if field_index < List.length sdef.fields
    then (
      let field = List.nth sdef.fields field_index in
      field)
    else raise (Failure "Psuedo-translate: Field index out of bounds")
  | None -> raise (Failure "Psuedo-translate: Struct not found")
;;

(* Translate SAST -> SAST, but convert all struct literals to dot assignments and unops to binop+assignments. Must happen after
   linearity checker, so dot accesses to linears still allowed *)

let eliminate_psuedo_nodes program =
  let globals, structs, funcs = program in
  (* Generate struct_info *)
  let struct_info = gen_struct_info structs in
  (* Translate an expression -- is_named indicates if expr ends up assigned to a name *)
  let rec translate_sexpr (sexpr : sexpr) (is_named : bool) : sexpr =
    (* Translate operation: recursively call on sub exprs, and expand pre/post into assignment *)
    let translate_operation (op : soperation) : sx =
      let expand_incop (incop : unArithOp) (e : sexpr) : sx =
        let typ, e = e in
        match e with
        | SId name ->
          (match incop with
           | PreInc ->
             (* x = *)
             SAssignment
               (SAssign
                  ( (typ, SId name)
                  , (* x + 1 *)
                    ( typ
                    , SOperation
                        (SArithOp ((typ, SId name), Add, (typ, SLiteral (SIntLit 1)))) )
                  ))
           | PostInc -> raise (Failure "Post increment not yet supported")
           | PreDec ->
             (* x = *)
             SAssignment
               (SAssign
                  ( (typ, SId name)
                  , (* x + 1 *)
                    ( typ
                    , SOperation
                        (SArithOp ((typ, SId name), Sub, (typ, SLiteral (SIntLit 1)))) )
                  ))
           | PostDec -> raise (Failure "Post decrement not yet supported")
           | Neg -> raise (Failure "Invalid state: Neg in expand_incop"))
        | _ -> raise (Failure "Can only increment/decrement variables")
      in
      match op with
      (* Unchanged *)
      | SDeref name -> SOperation (SDeref name)
      | SBorrow name -> SOperation (SBorrow name)
      | SAccessOp (n1, op, n2) -> SOperation (SAccessOp (n1, op, n2))
      (* Just recursively apply *)
      | SArithOp (e1, op, e2) ->
        SOperation (SArithOp (translate_sexpr e1 false, op, translate_sexpr e2 false))
      | SCompOp (e1, op, e2) ->
        SOperation (SCompOp (translate_sexpr e1 false, op, translate_sexpr e2 false))
      | SLogOp (e1, op, e2) ->
        SOperation (SLogOp (translate_sexpr e1 false, op, translate_sexpr e2 false))
      | SUnArithOp (Neg, e) -> SOperation (SUnArithOp (Neg, translate_sexpr e false))
      | SUnLogOp (Not, e) -> SOperation (SUnLogOp (Not, translate_sexpr e false))
      (* Translate to assignment *)
      | SUnArithOp (incop, e) -> expand_incop incop e
    in
    (* Translate assignments by translating expressions, marking rhs as named *)
    let translate_assignment (ass : sassignment) : sassignment =
      match ass with
      | SAssign (e1, e2) -> SAssign (translate_sexpr e1 false, translate_sexpr e2 true)
      | SDerefAssign (n1, e2) -> SDerefAssign (n1, translate_sexpr e2 true)
      | SStructAssign (n1, n2, e3) -> SStructAssign (n1, n2, translate_sexpr e3 true)
      | SRefStructAssign (n1, n2, e3) -> SRefStructAssign (n1, n2, translate_sexpr e3 true)
      | SStructExplode (ns, e2) -> SStructExplode (ns, translate_sexpr e2 true)
    in
    let translate_literal (lit : sliteral) : sliteral =
      match lit with
      | SArrayLit _ -> raise (Failure "Arrays not supported")
      (* Should already be translated by this point *)
      | SStructLit (sname, sexprs) ->
        raise
          (Failure
             "Struct literal translation failed: unnamed struct literals not yet \
              implemented")
      | other -> other
    in
    let typ, sexpr = sexpr in
    ( typ
    , match sexpr with
      | SId name -> SId name
      | SLiteral lit -> SLiteral (translate_literal lit)
      | SOperation op -> translate_operation op
      | SAssignment ass -> SAssignment (translate_assignment ass)
      | SCall (name, args) ->
        SCall (name, List.map (fun e -> translate_sexpr e false) args) )
    (* Translate a statement *)
  in
  let rec translate_statement (stmt : sstmt) : sstmt =
    match stmt with
    | SBlock (decls, stmts) -> SBlock (decls, List.map translate_statement stmts)
    | FSBlock (decls, stmts, ret_stmt) -> FSBlock (decls, List.map translate_statement stmts, ret_stmt)
    (* Special struct literal translation can translate an expression into a statements *)
    | SExpr
        ( typ
        , SAssignment
            (SAssign
              ((_, SId name), (rhs_typ, SLiteral (SStructLit (sname, sexpr_list))))) ) ->
      (* Translate into block *)
      SBlock
        ( [] (* No decls *)
        , List.mapi
            (fun n sexpr ->
              let ftyp, fname = get_field sname n struct_info in
              SExpr (ftyp, SAssignment (SStructAssign (name, fname, sexpr))))
            sexpr_list )
    | SExpr sexpr -> SExpr (translate_sexpr sexpr false)
    | SIf (cond, s1, s2) -> SIf (cond, translate_statement s1, translate_statement s2)
    | SWhile (cond, s) -> SWhile (cond, translate_statement s)
    | SContinue | SBreak -> raise (Failure "continue and break not supported")
  in
  (* Translate single function *)
  let translate_function (func : sfunc_def) : sfunc_def =
    (* Leave everything the same except sbody and sreturn *)
    let translated_body = { func with sbody = List.map translate_statement func.sbody } in
    { translated_body with
      sreturn =
        (match translated_body.sreturn with
         | SReturn sexpr -> SReturn (translate_sexpr sexpr false)
         | SVoidReturn -> SVoidReturn)
    }
  in

  (globals, structs, List.map translate_function funcs)
;;
