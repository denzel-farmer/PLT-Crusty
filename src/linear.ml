open Ast
open Sast
open Astprint
open Sastprint
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

(* Print functions for debugging and error messages *)
let linear_log_level = Some Debug

let log_print (msg : string) (level : log_level) : unit =
  match linear_log_level, level with
  | Some Debug, Debug | Some Debug, Info -> print_string msg
  | Some Info, Info -> print_string msg
  | Some Info, Debug -> ()
  | None, _ -> ()
;;

let log_println (msg : string) (level : log_level) : unit = log_print (msg ^ "\n") level
let debug_print (msg : string) : unit = log_print msg Debug
let debug_println (msg : string) : unit = log_println ("[DEBUG] " ^ msg) Debug
let info_print (msg : string) : unit = log_print msg Info
let info_println (msg : string) : unit = log_println ("[INFO] " ^ msg) Info

let string_of_struct_info (struct_info : struct_info) : string =
  if StringMap.is_empty struct_info
  then "[]"
  else
    StringMap.fold
      (fun key value acc -> acc ^ key ^ " -> " ^ string_of_struct_def value ^ "\n")
      struct_info
      ""
;;

let string_of_func_info (func_info : func_info) : string =
  if StringMap.is_empty func_info
  then "[]"
  else
    StringMap.fold
      (fun key value acc -> acc ^ key ^ " -> " ^ string_of_sfunc_def value ^ "\n")
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
        ^ string_of_typ (snd value)
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
  match in_struct with
  | { lin_qual = Linear } -> true
  | _ -> false
;;

(* Given a struct map and a struct name, search the struct map for that struct *)
let get_struct (struct_map : struct_info) (sname : string) : struct_def option =
  StringMap.find_opt sname struct_map
;;

(* Linear Type Helpers *)

(* check if a given variable type is linear (based on struct_map if it is a struct type)
   if struct type and struct type not present in map, returns false *)
let rec is_linear_type (struct_map : struct_info) (var_type : typ) : bool =
  match var_type with
  | Prim (Linear, _) -> true
  | Struct sname ->
    (match get_struct struct_map sname with
     | Some in_struct -> is_linear_struct in_struct
     | None -> false)
  | Arr (subtyp, _) ->
    is_linear_type struct_map subtyp
    (* Array of linear types is linear *)
    (* TODO add linear arrays of unrestricted types *)
  | Ref _ -> false (* Refs can't be linear *)
  | _ -> false
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
    | _ -> false
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

(* Takes in a function that takes in a linear_map, and converts it to a function
   that takes in a linear_map_result *)

let try_lin_map (input : linear_map_result) (f : linear_map -> linear_map_result)
  : linear_map_result
  =
  match input with
  | Error err -> Error err
  | Ok lin_map -> f lin_map
;;

let merge_map (map1 : linear_map_result) (map2 : linear_map_result) : linear_map_result =
  (* TODO implement *)
  map1
;;

(* TODO implement - this is 'top level' block checking function that includes checking helpers.
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
      | None -> Error ("Variable " ^ ident ^ " not declared")
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
    (* Check an assignment expression *)
    let check_assignment (lin_map : linear_map) (assmt : sassignment) : linear_map_result =
      info_println "Checking assignment expression";
      debug_println ("Assignment is  \"" ^ string_of_sassignment assmt ^ "\"");
      match assmt with
      | SAssign (id, expr) ->
        (* Regular assignment, mark lhs assigned and check rhs, with 'is_consumed' context *)
        let lin_map = assign_ident lin_map id in
        check_expr lin_map true expr
      | SStructAssign (s_id, mem_id, expr) ->
        (* Assignment to a struct member (ex. point.x = 5)*)
        Ok lin_map
      | SRefStructAssign (s_ref_id, mem_id, expr) -> Ok lin_map
      | SStructExplode (idents, s_expr) -> Ok lin_map
    in
    (* Check expression *)
    info_println "Checking expression";
    debug_println ("Expression is  \"" ^ string_of_sexpr expr ^ "\"");
    match lin_map with
    | Error err -> Error err
    | Ok lin_map ->
      (match expr with
       | _, SLiteral _ -> (* Literals have no rules *) Ok lin_map
       | id_typ, SId id ->
         (* An expression with just an identifier *)
         check_lone_ident lin_map is_consumed id
       | out_typ, SOperation op -> Ok lin_map
       | out_typ, SAssignment assmt -> check_assignment lin_map assmt
       | out_typ, SCall (fname, args) -> Ok lin_map)
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
    (* I removed merging here, because I think we should do that in
       the outer level check block, but I could be wrong*)
  in
  (* Actual body of check_block: *)
  (*One possible way to do this that isn't saving/comparing the lin map:
    - save new decls in a list
    - add new decls to lin map
    - check statements
    - remove new decls from lin map one by one, checking that none are unconsumed
    - return lin map
  *)

  (*Basic implementation that only works for one level of scope *)
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
    info_println "Done checking block";
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
    | SReturn ex -> func.sbody @ [ SExpr ex ]
  in
  info_println "Checking function body";
  let lin_map =
    linear_check_block struct_info func_info (Ok lin_map) func.slocals func_statements
  in
  debug_println
    ("Final lin_map for " ^ func.sfname ^ string_of_result string_of_linear_map lin_map);
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
