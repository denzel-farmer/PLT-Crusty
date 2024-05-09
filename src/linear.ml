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

(* Print functions for debugging and error messages *)
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
  "Program Info: "
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

(* Function checking stuff *)

(* Check an expression *)
let rec check_expr
  (struct_info : struct_info)
  (func_info : func_info)
  (lin_map : linear_map_result)
  (expr : sexpr)
  : linear_map_result
  =
  lin_map
;;

(* TODO: I just went through and threw in whatever values made these functions not throw
   compile errors, they are still definitely wrong - denzel*)
let linear_add_locals
  (struct_info_map : struct_info)
  (lin_map : linear_map)
  (slocals : var_decl list)
  : linear_map
  =
  (*TODO implement *)
  lin_map
;;

let linear_add_args
  (struct_info_map : struct_info)
  (lin_map : linear_map)
  (args : var_decl list)
  : linear_map
  =
  let add_arg (lin_map : linear_map) ((typ, arg_name) as arg_decl : var_decl) : linear_map
    =
    (* TODO check for duplicate entries?? *)
    (* if linear, add arg *)
    match is_linear_decl struct_info_map arg_decl with
    | false -> lin_map
    | true -> StringMap.add arg_name (Assigned, typ) lin_map
  in
  let new_map = List.fold_left add_arg lin_map args in
  new_map
;;

let merge_map (map1 : linear_map_result) (map2 : linear_map_result) : linear_map_result =
  (* TODO implement *)
  map1
;;

let rec linear_check_stmt_list
  (struct_info : struct_info)
  (func_info : func_info)
  (in_lin_map : linear_map_result)
  (body : sstmt list)
  : linear_map_result
  =
  (* TODO implement *)
  let linear_check_block (s_list : sstmt list) (lin_map : linear_map_result)
    : linear_map_result
    =
    lin_map
  in
  let rec linear_check_stmt (lin_map : linear_map_result) (stmt : sstmt)
    : linear_map_result
    =
    match stmt with
    | SBlock s_list -> linear_check_block s_list lin_map
    | SExpr ex -> check_expr struct_info func_info lin_map ex
    | SIf (cond_ex, true_stmt, false_stmt) ->
      let lin_map = check_expr struct_info func_info lin_map cond_ex in
      let true_map = linear_check_stmt lin_map true_stmt in
      let false_map = linear_check_stmt lin_map false_stmt in
      merge_map true_map false_map
    | SWhile (cond_ex, body_stmt) ->
      let lin_map = check_expr struct_info func_info lin_map cond_ex in
      let end_map = linear_check_stmt lin_map body_stmt in
      merge_map lin_map end_map
    | SBreak | SContinue -> raise (Failure "Break and Continue not supported")
  in
  let new_map = List.fold_left linear_check_stmt in_lin_map body in
  merge_map in_lin_map new_map
;;

let linear_check_return
  (struct_info : struct_info)
  (func_info : func_info)
  (ret_statement : sreturn_stmt)
  (lin_map : linear_map_result)
  : linear_map_result
  =
  (* TODO implement *)
  lin_map
;;

(* Initial Information Gathering Funcs *)
let init_lin_map (struct_info : struct_info) (func : sfunc_def) : linear_map =
  let args = func.sargs in
  let lin_map = linear_add_args struct_info StringMap.empty args in
  let locals = func.slocals in
  linear_add_locals struct_info lin_map locals
;;

let ( let* ) = Result.bind

(* Check a function to ensure it follows linearity rules *)
let process_func (struct_info : struct_info) (func_info : func_info) (func : sfunc_def)
  : string * linear_map_result
  =
  (* TODO pull out all the error checking glue with cool monad stuff *)
  let lin_map = init_lin_map struct_info func in
  let lin_map = linear_check_stmt_list struct_info func_info (Ok lin_map) func.sbody in
  let lin_map = linear_check_return struct_info func_info func.sreturn lin_map in
  func.sfname, lin_map
;;

(* Check linearity on a program *)
let check program : linear_result =
  let _, structs, funcs = program in
  let program_info_maps = generate_program_info structs funcs in
  match program_info_maps with
  | Error err -> Error err, []
  | Ok (struct_info_map, func_info_map) ->
    (* check functions *)
    let lin_map = List.map (process_func struct_info_map func_info_map) funcs in
    Ok (struct_info_map, func_info_map), lin_map
;;

(* match sstruct.lin_qual with
   | Unrestricted ->
   (* check that sstruct.sfields doesn't have linear types else LinError *)
   | Linear ->
   (* add structdef to lin_map, then check sstruct.sfields for any linear types *)
   Hashtbl.add lin_map arg_name (_, Assigned, typ); *)

(*
   let rec linear_check_func (lin_map : linear_map) (func : sfunc_def) : linear_result =
   (* Add any linear arguments to lin_map as Assigned
   If any duplicate linear args, return an error (TODO probably put this in more general checking function)
   *)
   let lin_map, linear_result = linear_add_args lin_map func.sargs in
   (*TODO this structure is dumb, use a monad? *)
   if (linear_result = Pass) then

   (* Add any linear locals to lin_map as Unassigned
   If any duplicate linear locals, or linear locals that conflict with args, return an error
   TODO probably put this in a more general checking function that also does symbol tables *)
   let lin_map, linear_result = linear_add_locals lin_map func.slocals in

   if (linear_result = Pass) then

   (* Check linearity on each statement in the function body, passing in lin_map *)
   let lin_map, linear_result = linear_check_stmt_list lin_map func.sbody in

   if (linear_result = Pass) then

   (* Check linearity on the return type *)
   let lin_map, linear_result = linear_check_stmt_list lin_map func.sret in

   if (linear_result = Pass) then LinPass
   else lin_map, linear_result
   else lin_map, linear_result
   else lin_map, linear_result
   else lin_map, linear_result *)

(* Check linearity on each statement in the function body, passing in lin_map *)

(* Do a check on the return type? *)

(* let check (program : sprogram) : bool result =
   (* Initialize the linear map object *)
   (* let lin_map = Hashtbl.create 0 in  *)

   let struct_results = linear_check_structs program.structs in

   Ok true
   ;; *)
(*
   let struct_results = List.map (linear_check_struct lin_map) program.sstructs in *)
(*
   (* Call linearity checking on each function, passing linear vars object to each (probably not needed) *)
   let func_results = List.map (linear_check_func lin_map) program.sfuncs in *)

(* If any function fails linearity checking, return the first error, otherwise pass
   match List.find_opt ((=) Error) struct_results with
   | Some (Error err) -> Error err
   | Some (Ok _) -> Error "Unexpected Ok in struct_results" (*TODO this is dumb*)
   | None -> Ok true
*)
