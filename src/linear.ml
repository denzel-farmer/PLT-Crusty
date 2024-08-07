open Ast
open Sast
open Log
module StringMap = Map.Make (String)

(* Types for passing results and errors *)

type struct_info = struct_def StringMap.t
type struct_info_result = (struct_info, string) result
type func_info = sfunc_def StringMap.t
type program_info = struct_info * func_info
type program_info_result = (program_info, string) result

(* Linear State Types *)

(* Linear state for a variable *)

type linear_state =
  | Unassigned
  | Assigned
  | Borrowed
  | Used
  | Ref

type linear_map = (linear_state * typ) StringMap.t
type linear_map_result = (linear_map, string) result

(* Final output type for debugging *)
type linear_result = program_info_result * (string * linear_map_result) list

type log_level =
  | Debug
  | Info

let lin_result_get_reason (lin_result : linear_result) : string option =
  let program_info_result, lin_map_result_tuples = lin_result in
  match program_info_result with
  | Error reason -> Some reason
  | Ok _ ->
    let lin_map_result =
      List.find_opt
        (fun (_, lin_map_result) ->
          match lin_map_result with
          | Error _ -> true
          | Ok _ -> false)
        lin_map_result_tuples
    in
    (match lin_map_result with
     | Some (_, Error reason) -> Some reason
     | Some (_, Ok _) -> raise (Failure "Lin result get reason did something weird")
     | None -> None)
;;

let lin_result_failed (lin_result : linear_result) : bool =
  match lin_result with
  | Error _, _ -> true
  | Ok _, lin_map_results ->
    List.exists
      (fun (_, lin_map_result) ->
        match lin_map_result with
        | Error _ -> true
        | Ok _ -> false)
      lin_map_results
;;

let string_of_struct_info (struct_info : struct_info) : string =
  if StringMap.is_empty struct_info
  then "[]"
  else
    StringMap.fold
      (fun key value acc ->
        acc ^ key ^ " -> " ^ Astprint.string_of_struct_def value ^ "\n")
      struct_info
      ""
;;

let string_of_func_info (func_info : func_info) : string =
  if StringMap.is_empty func_info
  then "[]"
  else
    StringMap.fold
      (fun key value acc ->
        acc ^ key ^ " -> " ^ Sastprint.string_of_sfunc_def value ^ "\n")
      func_info
      ""
;;

let string_of_program_info (program_info : program_info) : string =
  let struct_info, func_info = program_info in
  "Struct Info:\n"
  ^ string_of_struct_info struct_info
  ^ "\nFunc Info:\n"
  ^ string_of_func_info func_info
;;

let string_of_linear_state (lin_state : linear_state) : string =
  match lin_state with
  | Unassigned -> "Unassigned"
  | Assigned -> "Assigned"
  | Borrowed -> "Borrowed"
  | Used -> "Used"
  | Ref -> "Ref"
;;

let string_of_linear_map (lin_map : linear_map) : string =
  if StringMap.is_empty lin_map
  then "[]"
  else
    StringMap.fold
      (fun key value acc ->
        acc
        ^ key
        ^ " -> "
        ^ string_of_linear_state (fst value)
        ^ " : "
        ^ Astprint.string_of_typ (snd value)
        ^ "\n")
      lin_map
      ""
;;

(* Generic Printing of 'Result' type *)
let string_of_result (string_of_a : 'a -> string) (result : ('a, string) result) : string =
  match result with
  | Ok a -> string_of_a a
  | Error err -> err
;;

let string_of_option (string_of_a : 'a -> string) (opt : 'a option) : string =
  match opt with
  | Some a -> string_of_a a
  | None -> "None"
;;

let string_of_linear_result (lin_result : linear_result) : string =
  let program_info_result, lin_map_result = lin_result in
  let result_header =
    if lin_result_failed lin_result then "LINEAR CHECK FAIL" else "LINEAR CHECK SUCCESS"
  in
  result_header
  ^ "\nProgram Info:\n"
  ^ string_of_result string_of_program_info program_info_result
  ^ "\nLinear Maps:\n"
  ^ List.fold_left
      (fun acc (fname, lin_map_res) ->
        acc ^ fname ^ " -> " ^ string_of_result string_of_linear_map lin_map_res ^ "\n")
      ""
      lin_map_result
;;

(* Generic Helpers *)

let flip f x y = f y x
let ( let* ) = Result.bind

(* Struct Info Helpers *)

(* Returns true if the given struct is linear, based on its qualifier *)
let is_linear_struct (in_struct : struct_def) : bool =
  let lin_qual = in_struct.lin_qual in
  match lin_qual with
  | Linear -> true
  | Unrestricted -> false
;;

(* Given a struct map and a struct name, search the struct map for that struct *)
let get_struct (struct_map : struct_info) (sname : string) : struct_def option =
  StringMap.find_opt sname struct_map
;;

(* Linear Type Helpers *)

(* check if a given variable type is linear (based on struct_map if it is a struct type)
   if struct type and struct type not present in map, returns false *)
let is_linear_type (struct_map : struct_info) (var_type : typ) : bool =
  match var_type with
  | Prim (Linear, _) -> true
  | Prim (Unrestricted, _) -> false
  | Struct sname ->
    (match get_struct struct_map sname with
     | Some in_struct -> is_linear_struct in_struct
     | None -> false)
  | Arr (subtyp, _) -> false (* Linear arrays not supported *)
  | Ref _ -> false (* Refs can't be linear *)
;;

let is_linear_decl (struct_map : struct_info) (decl : var_decl) : bool =
  is_linear_type struct_map (fst decl)
;;

(* Process a list of structs, populating a result map that maps struct name
   to struct definition. Also checks for unrestricted structs with linear members,
   and if they exist returns Error *)
let process_structs (structs : struct_def list) : struct_info_result =
  (* Check that a struct has only linear members, based on types and previous structs in struct_map.
     If it is Linear, add it to the map. *)
  let process_struct (in_struct : struct_def) (struct_map : struct_info)
    : struct_info_result
    =
    match in_struct.lin_qual with
    | Linear -> Ok (StringMap.add in_struct.sname in_struct struct_map)
    | Unrestricted ->
      if List.exists (is_linear_decl struct_map) in_struct.fields
      then Error ("Struct " ^ in_struct.sname ^ " has linear fields but is unrestricted")
      else Ok struct_map (* Ok, but don't add unrestricted struct to map*)
  in
  (* Fold over each struct, accumulating but returning error if any fails *)
  List.fold_left
    (fun acc in_struct -> Result.bind acc (process_struct in_struct))
    (Ok StringMap.empty)
    structs
;;

(* Take in a list of func prototypes and adds each to a map if they have a
   linear return type or linear arguments. Does no rule-checking. *)
let gen_func_info_map (funcs : sfunc_def list) (struct_info_map : struct_info) : func_info
  =
  let func_has_linear_args (func : sfunc_def) : bool =
    List.exists (is_linear_decl struct_info_map) func.sargs
  in
  let func_has_linear_ret (func : sfunc_def) : bool =
    match func.srtyp with
    | Nonvoid ret -> is_linear_type struct_info_map ret
    | Void -> false
  in
  List.fold_left
    (fun acc func ->
      if func_has_linear_args func || func_has_linear_ret func
      then StringMap.add func.sfname func acc
      else acc)
    StringMap.empty
    funcs
;;

let generate_program_info (structs : struct_def list) (funcs : sfunc_def list)
  : program_info_result
  =
  let struct_info_map = process_structs structs in
  match struct_info_map with
  | Ok struct_info -> Ok (struct_info, (gen_func_info_map funcs) struct_info)
  | Error err -> Error err
;;

(* Function checking stuff, TODO move everything inside process_func? *)

(* Add a declaration to the linear map with the given state, if it is linear,
   otherwise does nothing *)
let add_decl_if_linear
  (struct_info : struct_info)
  (state : linear_state)
  (lin_map : linear_map)
  (decl : var_decl)
  : linear_map
  =
  let typ, name = decl in
  if is_linear_type struct_info typ
  then StringMap.add name (state, typ) lin_map
  else lin_map
;;

(* Add declarations to linear map if they are linear, ignoring otherwise *)
let add_linear_decls
  (struct_info : struct_info)
  (state : linear_state)
  (lin_map : linear_map)
  (args : var_decl list)
  : linear_map
  =
  let new_map = List.fold_left (add_decl_if_linear struct_info state) lin_map args in
  new_map
;;

(* Removes declaration if Used *)
let remove_decl (lin_map : linear_map_result) (decl : var_decl) : linear_map_result =
  match lin_map with
  | Error err -> Error err
  | Ok lin_map ->
    let typ, name = decl in
    (match StringMap.find_opt name lin_map with
     | Some (Used, typ) ->
       info_println ("Found " ^ name ^ " with state Used");
       Ok (StringMap.remove name lin_map)
     | Some (_, typ) ->
       info_println ("Found " ^ name ^ " but is not Used");
       Error ("Declaration " ^ name ^ " is Unconsumed")
     | None ->
       debug_println ("Didn't find decl " ^ name ^ ", assuming unrestricted");
       Ok lin_map)
;;

(* removes new decls from linear map after checking block *)
let remove_linear_decls (lin_map : linear_map_result) (vdecls : var_decl list)
  : linear_map_result
  =
  match lin_map with
  | Error err -> Error err
  | Ok lin_map ->
    info_println "Removing locally declared variables";
    let new_map = List.fold_left remove_decl (Ok lin_map) vdecls in
    new_map
;;

(* Takes in a function that takes in a linear_map, and converts it to a function
   that takes in a linear_map_result *)

let try_lin_map (input : linear_map_result) (f : linear_map -> linear_map_result)
  : linear_map_result
  =
  match input with
  | Error err -> Error err
  | Ok lin_map -> f lin_map
;;

(* Merges two maps to check that the states of variables among two branches are equal *)
let merge_map (map1 : linear_map_result) (map2 : linear_map_result) : linear_map_result =
  (* TODO implement *)
  match map1, map2 with
  | Ok map1', Ok map2' ->
    info_println "Merging two branches";
    StringMap.fold
      (fun name state1 acc ->
        match acc with
        | Ok check_map ->
          (match StringMap.find_opt name map2' with
           | Some state2 ->
             let lin_state1, _ = state1 in
             let lin_state2, _ = state2 in
             if state1 = state2
             then (
               info_println
                 ("Variable "
                  ^ name
                  ^ " has same state "
                  ^ string_of_linear_state lin_state1);
               Ok check_map)
             else (
               debug_println
                 ("Variable "
                  ^ name
                  ^ " has unequal states "
                  ^ string_of_linear_state lin_state1
                  ^ " and "
                  ^ string_of_linear_state lin_state2);
               Error ("Variable " ^ name ^ " has unequal branches."))
           | None -> Error ("Variable " ^ name ^ " has unequal branches."))
        | Error _ -> acc)
      map1'
      (Ok map1')
  | Error err, _ -> Error err
  | _, Error err -> Error err
;;

(*  This is 'top level' block checking function that includes checking helpers.
   I put everything under this function so we could just pass in struct_info and func_info once
   and use them everywhere since they don't change rather than keep passing them to every subfunction *)
let rec linear_check_block
  (struct_info : struct_info)
  (func_info : func_info)
  (lin_map : linear_map_result)
  (vdecls : var_decl list)
  (s_list : sstmt list)
  : linear_map_result
  =
  (* Check an expression and transform linear_map. Bool is_consumed indicates
     whether this expression is immediately consumed *)
  let rec check_expr (lin_map : linear_map_result) (is_consumed : bool) (expr : sexpr)
    : linear_map_result
    =
    (* Try to consume an assigned identifier, throws an error if in the wrong state. Does
       nothing if not in lin_map. *)
    let consume_ident (lin_map : linear_map) (ident : string) : linear_map_result =
      info_println ("Marking " ^ ident ^ " as consumed");
      match StringMap.find_opt ident lin_map with
      | Some (Assigned, typ) -> Ok (StringMap.add ident (Used, typ) lin_map)
      | Some (Borrowed, typ) -> Ok (StringMap.add ident (Used, typ) lin_map)
      | Some (Unassigned, typ) ->
        Error ("Variable " ^ ident ^ " consumed before assignment")
      | Some (Used, typ) -> Error ("Variable " ^ ident ^ " consumed more than once")
      | Some (Ref, typ) -> Error ("Reference " ^ ident ^ " cannot be consumed")
      | None -> Ok lin_map
      (* TODO I am starting to think we should just add everything and double check here...*)
    in
    let assign_ident (lin_map : linear_map) (ident : string) : linear_map_result =
      info_println ("Marking " ^ ident ^ " as assigned");
      match StringMap.find_opt ident lin_map with
      | Some (Unassigned, typ) -> Ok (StringMap.add ident (Assigned, typ) lin_map)
      | Some (Used, typ) ->
        debug_println (ident ^ " is being re-asigned after use");
        Ok (StringMap.add ident (Assigned, typ) lin_map)
      | Some (Assigned, typ) -> Error ("Variable " ^ ident ^ " assigned more than once")
      | Some (Borrowed, typ) -> Error ("Variable " ^ ident ^ " borrowed before assignment")
      | Some (Ref, typ) -> Error ("Reference " ^ ident ^ " cannot be assigned")
      | None -> Ok lin_map
    in
    (* Check a single-identifier expression. If is_consume is true, this expression is
       immediately consumed (i.e. is passed as a function argument or assigned to a new variable).*)
    let check_lone_ident (lin_map : linear_map) (is_consumed : bool) (ident : string)
      : linear_map_result
      =
      debug_print
        ("Checking lone identifier \""
         ^ ident
         ^ "\" (is_consumed="
         ^ string_of_bool is_consumed
         ^ ")\n");
      if is_consumed
      then consume_ident lin_map ident
      else (
        match StringMap.find_opt ident lin_map with
        | None ->
          (* If identifier is unrestricted, we don't care *)
          Ok lin_map
        | Some (Unassigned, _) ->
          (* Whether or not it is explosion or psuedo-dot access, this is an error *)
          Error ("Linear variable \"" ^ ident ^ "\" used before assignment")
        | Some (Used, _) ->
          (* Whether or not it is explosion or psuedo-dot access, this is an error *)
          Error ("Linear variable \"" ^ ident ^ "\" used more than once")
        | Some (Assigned, Prim (Linear, _)) | Some (Borrowed, Prim (Linear, _)) ->
          (* The only time a non-consumed, lone linear identifier is allowed is if it
             is an assigned primitive, in which case we treat this as sugar for dot access *)
          Ok lin_map
        | _ ->
          (* Otherwise, silent discard is happening *)
          Error ("Linear variable " ^ ident ^ " cannot be silently discarded"))
    in
    (*TODO make helper function for parts of var -> struct def -> member accesses *)
    (* Check an assignment expression *)
    let check_assignment (lin_map : linear_map) (assmt : sassignment) : linear_map_result =
      info_println "Checking assignment expression";
      debug_println ("Assignment is  \"" ^ Sastprint.string_of_sassignment assmt ^ "\"");
      match assmt with
      | SAssign (id, expr) ->
        (match id with
         (* | _, SOperation (SDeref ref_name) -> Ok lin_map *)
         | _, SId id ->
           (* Regular assignment, mark lhs assigned and check rhs, with 'is_consumed' context *)
           let lin_map = check_expr (Ok lin_map) true expr in
           (match lin_map with
            | Error err -> Error err
            | Ok lin_map -> assign_ident lin_map id)
         | _ -> raise (Failure "Linear checker error: assigning to invalid expression"))
      | SStructAssign (s_id, mem_id, expr) ->
        (* Check expression with is_consumed true *)
        let lin_map = check_expr (Ok lin_map) true expr in
        (match lin_map with
         | Error err -> Error err
         | Ok lin_map ->
           (* Look up identifier name in lin_map to get struct name *)
           (match StringMap.find_opt s_id lin_map with
            | None ->
              debug_println ("Variable " ^ s_id ^ " not found, assuming unrestricted");
              Ok lin_map
            | Some (Ref, _) -> Error ("Cannot dot-assign value to reference " ^ s_id)
            | Some (Unassigned, _) ->
              Error
                ("Cannot assign members of linear variable " ^ s_id ^ " before assignment")
            | Some (Used, _) ->
              Error ("Cannot assign members of linear variable " ^ s_id ^ " after use")
            | Some (Assigned, Struct struct_name) | Some (Borrowed, Struct struct_name) ->
              (* Check member is not linear *)
              let struct_def = get_struct struct_info struct_name in
              (match struct_def with
               | None ->
                 debug_println ("Struct " ^ s_id ^ " not found, assuming unrestricted");
                 Ok lin_map
               | Some struct_def ->
                 (* check mem is not linear *)
                 let field =
                   List.find (fun (typ, name) -> name = mem_id) struct_def.fields
                 in
                 if is_linear_type struct_info (fst field)
                 then
                   Error
                     ("Cannot assign value to linear struct field " ^ s_id ^ "." ^ mem_id)
                 else Ok lin_map)
            | Some (Assigned, _) | Some (Borrowed, _) ->
              raise (Failure "Linear checker error: struct dot-assignment to non-struct")))
      | SRefStructAssign (s_ref_id, mem_id, expr) ->
        (* Consume RHS *)
        let lin_map = check_expr (Ok lin_map) true expr in
        (match lin_map with
         | Error err -> Error err
         | Ok lin_map ->
           (* If lhs is in lin_map, raise error *)
           (match StringMap.find_opt s_ref_id lin_map with
            | None -> Ok lin_map
            | Some _ -> raise (Failure ("Linear references are read only: " ^ s_ref_id))))
      | SStructExplode (idents, s_expr) ->
        (* Try to mark each argument as assigned *)
        let lin_map =
          (* Check the right-hand side, noting that it is consumed *)
          check_expr (Ok lin_map) true s_expr
        in
        List.fold_left
          (fun acc id ->
            match acc with
            | Error err -> Error err
            | Ok lin_map -> assign_ident lin_map id)
          lin_map
          idents
      | SDerefAssign (deref_id, expr) ->
        (* Consume RHS *)
        let lin_map = check_expr (Ok lin_map) true expr in
        (match lin_map with
         | Error err -> Error err
         | Ok lin_map ->
           (* If lhs is in lin_map, raise error *)
           (match StringMap.find_opt deref_id lin_map with
            | None -> Ok lin_map
            | Some _ -> raise (Failure ("Linear references are read only: " ^ deref_id))))
    in
    let check_func_call (lin_map : linear_map) (fname : string) (args : sexpr list)
      : linear_map_result
      =
      info_println ("Checking function call to " ^ fname);
      debug_println ("Function call args: " ^ Sastprint.string_of_sexpr_list args);
      (* Check each function arguement; if linear, it gets consumed *)
      let lin_map =
        List.fold_left (fun acc arg -> check_expr acc true arg) (Ok lin_map) args
      in
      (* Check function return type *)
      match StringMap.find_opt fname func_info with
      | None ->
        (* Function is not linear, we don't care about its return type *)
        lin_map
      | Some def ->
        (* Function is linear, check return type *)
        (match def.srtyp with
         | Void -> lin_map
         | Nonvoid ret_typ ->
           if is_linear_type struct_info ret_typ && not is_consumed
           then
             (* Function returns linear but value is not consumed, silent discard*)
             Error ("Function " ^ fname ^ " returns linear value that is not consumed")
           else lin_map)
    in
    let check_operation (lin_map : linear_map) (oper : soperation) : linear_map_result =
      match oper with
      (* For regular operations, just check any operands with is_consmed = false *)
      | SArithOp (operand1, _, operand2) ->
        let lin_map = check_expr (Ok lin_map) false operand1 in
        check_expr lin_map false operand2
      | SUnArithOp (_, operand) -> check_expr (Ok lin_map) false operand
      | SCompOp (operand1, _, operand2) ->
        let lin_map = check_expr (Ok lin_map) false operand1 in
        check_expr lin_map false operand2
      | SLogOp (operand1, _, operand2) ->
        let lin_map = check_expr (Ok lin_map) false operand1 in
        check_expr lin_map false operand2
      | SUnLogOp (_, operand) -> check_expr (Ok lin_map) false operand
      | SAccessOp (s_id, Dot, mem_id) ->
        (* Look up identifier name in lin_map to get struct name *)
        (match StringMap.find_opt s_id lin_map with
         | None ->
           debug_println ("Variable " ^ s_id ^ " not found, assuming unrestricted");
           Ok lin_map
         | Some (Ref, _) -> Error ("Cannot dot-access value from reference " ^ s_id)
         | Some (Unassigned, _) ->
           Error
             ("Cannot access members of linear variable " ^ s_id ^ " before assignment")
         | Some (Used, _) ->
           Error ("Cannot access members of linear variable " ^ s_id ^ " after use")
         | Some (Assigned, Struct struct_name) | Some (Borrowed, Struct struct_name) ->
           (* Check member is not linear *)
           let struct_def = get_struct struct_info struct_name in
           (match struct_def with
            | None ->
              (* TODO maybe this should raise an error *)
              debug_println ("Struct " ^ s_id ^ " not found, assuming unrestricted");
              Ok lin_map
            | Some struct_def ->
              (* check mem is not linear *)
              let field =
                List.find (fun (typ, name) -> name = mem_id) struct_def.fields
              in
              if is_linear_type struct_info (fst field)
              then
                Error ("Cannot access value of linear struct field " ^ s_id ^ "." ^ mem_id)
              else Ok lin_map)
         | Some (Assigned, _) | Some (Borrowed, _) ->
           raise (Failure "Linear checker error: struct dot-access from non-struct"))
      | SAccessOp (s_id, Arrow, mem_id) ->
        (* Look up identifier name in lin_map to get struct name *)
        (match StringMap.find_opt s_id lin_map with
         | None ->
           debug_println ("Variable " ^ s_id ^ " not found, assuming unrestricted");
           Ok lin_map
         | Some (Ref, Ref (Struct struct_name)) ->
           (* Check member is not linear *)
           let struct_def = get_struct struct_info struct_name in
           (match struct_def with
            | None ->
              debug_println ("Struct " ^ s_id ^ " not found, assuming unrestricted");
              Ok lin_map
            | Some struct_def ->
              (* check mem is not linear *)
              let field =
                List.find (fun (typ, name) -> name = mem_id) struct_def.fields
              in
              if is_linear_type struct_info (fst field)
              then
                Error ("Cannot arrow access linear struct field " ^ s_id ^ "->" ^ mem_id)
              else Ok lin_map)
         | Some (Ref, _) ->
           raise
             (Failure ("Linear checker error: struct arrow access to non-struct " ^ s_id))
         | Some _ -> Error ("Can only arrow-access references, var: " ^ s_id))
      | SDeref ref_id ->
        (* Just need to check if in lin_map, if so fail *)
        (match StringMap.find_opt ref_id lin_map with
         | None ->
           debug_println "Not in lin_map, must be unrestricted dereference";
           Ok lin_map
         | Some _ -> Error ("Cannot dereference linear reference " ^ ref_id))
      | SBorrow var_id ->
        (* Get var name in lin map *)
        (match StringMap.find_opt var_id lin_map with
         | None ->
           debug_println "Not in lin_map, must be unrestricted borrow";
           Ok lin_map
         | Some (Assigned, typ) | Some (Borrowed, typ) ->
           (* Must be borrowing something linear, requires consumption *)
           if not is_consumed
           then Error ("Cannot borrow linear " ^ var_id ^ " unless at call site")
           else (
             (* Update to have state Borrowed *)
             debug_println ("Marking var" ^ var_id ^ "as Borrowed");
             Ok (StringMap.add var_id (Borrowed, typ) lin_map))
         | Some (Unassigned, _) -> Error ("Cannot borrow " ^ var_id ^ " before assignment")
         | Some (Ref, _) -> Error ("Cannot borrow reference " ^ var_id)
         | Some (Used, _) -> Error ("Cannot borrow " ^ var_id ^ " after use"))
    in
    (* Check expression *)
    info_println "Checking expression";
    debug_println ("Expression is  \"" ^ Sastprint.string_of_sexpr expr ^ "\"");
    match lin_map with
    | Error err -> Error err
    | Ok lin_map ->
      (match expr with
       | _, SLiteral (SStructLit (sname, fields)) ->
         (* Struct literals have some rules, since they can contain expressions *)
         (* Check each expression, marking as consumed only if struct overall is consumed *)
         List.fold_left
           (fun acc expr -> check_expr acc is_consumed expr)
           (Ok lin_map)
           fields
       | _, SLiteral _ -> (* Other literals have no rules *) Ok lin_map
       | id_typ, SId id ->
         (* An expression with just an identifier *)
         check_lone_ident lin_map is_consumed id
       | out_typ, SOperation op -> check_operation lin_map op
       | out_typ, SAssignment assmt -> check_assignment lin_map assmt
       | out_typ, SCall (fname, args) -> check_func_call lin_map fname args)
  in
  (* Check a list of statements *)
  let rec linear_check_stmt_list (in_lin_map : linear_map_result) (s_list : sstmt list)
    : linear_map_result
    =
    let rec linear_check_stmt (lin_map : linear_map_result) (stmt : sstmt)
      : linear_map_result
      =
      match stmt with
      | SBlock (vdecls, stmts) ->
        linear_check_block struct_info func_info lin_map vdecls stmts
      | FSBlock (vdecls, stmts, ret_stmt) -> 
        linear_check_block struct_info func_info lin_map vdecls stmts
      | SExpr ex -> check_expr lin_map false ex
      | SIf (cond_ex, true_stmt, false_stmt) ->
        let lin_map = check_expr lin_map false cond_ex in
        let true_map = linear_check_stmt lin_map true_stmt in
        let false_map = linear_check_stmt lin_map false_stmt in
        merge_map true_map false_map
      | SWhile (cond_ex, body_stmt) ->
        let lin_map = check_expr lin_map false cond_ex in
        let end_map = linear_check_stmt lin_map body_stmt in
        merge_map lin_map end_map
      | SBreak | SContinue -> raise (Failure "Break and Continue not supported")
    in
    List.fold_left linear_check_stmt in_lin_map s_list
  in
  (* Actual body of check_block: *)
  info_println "Checking new block";
  match lin_map with
  | Error err -> Error err
  | Ok lin_map ->
    (* Add local variables *)
    let lin_map = add_linear_decls struct_info Unassigned lin_map vdecls in
    debug_println ("Added locals to lin_map: " ^ string_of_linear_map lin_map);
    (* Check statements *)
    let lin_map = linear_check_stmt_list (Ok lin_map) s_list in
    debug_println
      ("Checked statements, lin_map: " ^ string_of_result string_of_linear_map lin_map);
    let lin_map = remove_linear_decls lin_map vdecls in
    lin_map
;;

(* Check a function to ensure it follows linearity rules, return
   a tuple of function name and the final lin_map *)
let process_func (struct_info : struct_info) (func_info : func_info) (func : sfunc_def)
  : string * linear_map_result
  =
  info_println ("Checking linearity of function: " ^ func.sfname);
  (* TODO pull out all the error checking glue with cool monad stuff *)
  let lin_map = StringMap.empty in
  let lin_map = add_linear_decls struct_info Assigned lin_map func.sargs in
  debug_println ("Added args to lin_map: " ^ string_of_linear_map lin_map);
  let func_statements =
    match func.sreturn with
    | SVoidReturn -> func.sbody
    | SReturn ex ->
      (* This is super hacky, treat a return statement like a call to a fake function named
         "return" which returns an unrestricted int--hopefully nobody makes their own
         linear function named "return"...*)
      func.sbody @ [ SExpr (Prim (Unrestricted, Int), SCall ("return", [ ex ])) ]
  in
  info_println "Checking function body";
  let lin_map =
    linear_check_block struct_info func_info (Ok lin_map) func.slocals func_statements
  in
  debug_println
    ("Final lin_map for "
     ^ func.sfname
     ^ " "
     ^ string_of_result string_of_linear_map lin_map);
  func.sfname, lin_map
;;

(* Check linearity on a program *)
let check program : linear_result =
  info_println "Begin checking linearity, generating program info";
  let _, structs, funcs = program in
  let program_info = generate_program_info structs funcs in
  info_println "Generated program info, checking linearity of functions";
  match program_info with
  | Error err -> Error err, []
  | Ok (struct_info, func_info) ->
    (* check functions *)
    let lin_map = List.map (process_func struct_info func_info) funcs in
    info_println "Done checking linearity";
    Ok (struct_info, func_info), lin_map
;;
