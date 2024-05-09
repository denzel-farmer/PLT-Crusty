open Ast
open Sast
open Astprint
open Sastprint
module StringMap = Map.Make (String)

(* Types for passing results and errors *)

type struct_info = struct_def StringMap.t
type struct_info_result = (struct_info, string) result
type func_info = sfunc_def StringMap.t

type linear_state =
  | Unassigned
  | Assigned
  | Borrowed
  | Used
  | Ref

type linear_map = (linear_state * typ) StringMap.t
type linear_map_result = (linear_map, string) result

(* Final output type for debugging *)
type linear_result = struct_info_result * func_info option * linear_map_result

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
  let struct_info, func_info, lin_map = lin_result in
  "Struct Info:\n"
  ^ string_of_result string_of_struct_info struct_info
  ^ "\nFunc Info:\n"
  ^ string_of_option string_of_func_info func_info
  ^ "\nLinear Map:\n"
  ^ string_of_result string_of_linear_map lin_map
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

(* let linear_add_locals (lin_map : linear_map) (slocals : sfunc_def.slocals) : linear_map result =
   (*TODO implement *)
   Ok lin_map

   let linear_add_args (lin_map : linear_map) (args : sfunc_def.sargs) : linear_map result =
   let add_arg (ref_qual, (linear_qual, typ, arg_name)) (lin_map, linear_result) =
   (* TODO check for duplicate entries?? *)
   (* if linear, add arg *)
   match linear_qual with
   | Unrestricted ->
   lin_map, LinPass
   | Linear ->
   Hashtbl.add lin_map arg_name (ref_qual, Assigned, typ);
   lin_map, LinPass
   in
   let new_map, linear_result = List.fold_left add_arg (lin_map, LinPass) args in
   new_map, linear_result
   in

   let merge_map (map1 : linear_map) (map2 : linear_map) : linear_map result =

   let rec linear_check_stmt_list (lin_map : linear_map) (body : sfunc_def.sbody) : linear_map result =
   (* TODO implement *)
   let linear_check_stmt (lin_map : linear_map) (list : stmt list) : linear_map =
   match stmt with
   | Block ->
   let linear_check_block
   | Expr ->
   linear_check_expr stmt
   | If ->
   let true_map = linear_check_stmt list;
   let false_map = linear_check_stmt list;
   merge_map true_map false_map
   | While ->
   let end_map = linear_check_stmt list;
   merge_map lin_map end_map
   in
   let new_map, linear_result = List.fold_left linear_check_stmt body lin_map;
   merge_map lin_map new_map
   Ok lin_map

   let rec linear_check_expr () : linear_map result
   (* TODO implement *)
   Ok lin_map

   let linear_check_return (lin_map :

   (* Hash table that maps struct name to struct definition *)
   (* let string_of_linear_result = function
   | LinearError s -> s
   | LinPass -> "Pass"
   ;; *)
   (* Hash table that maps name to linearity and type information *)
   (* type linear_map = (string, (ref_qual * linear_state * stype)) Hashtbl.t *)
   (*
   let rec linear_add_locals (lin_map : linear_map) (slocals : sfunc_def.slocals) : linear_map result =
   (*TODO implement *)
   Ok lin_map

   let linear_add_args (lin_map : linear_map) (args : sfunc_def.sargs) : linear_map result =
   let add_arg (ref_qual, (linear_qual, typ, arg_name)) (lin_map, linear_result) =
   (* TODO check for duplicate entries?? *)
   (* if linear, add arg *)
   match linear_qual with
   | Unrestricted ->
   lin_map, LinPass
   | Linear ->
   Hashtbl.add lin_map arg_name (ref_qual, Assigned, typ);
   lin_map, LinPass
   in
   let new_map, linear_result = List.fold_left add_arg (lin_map, LinPass) args in
   new_map, linear_result
   in

   let merge_map (map1 : linear_map) (map2 : linear_map) : linear_map result =

   let rec linear_check_stmt_list (lin_map : linear_map) (body : sfunc_def.sbody) : linear_map result =
   (* TODO implement *)
   let linear_check_stmt (lin_map : linear_map) (list : stmt list) : linear_map =
   match stmt with
   | Block ->
   let linear_check_block
   | Expr ->
   linear_check_expr stmt
   | If ->
   let true_map = linear_check_stmt list;
   let false_map = linear_check_stmt list;
   merge_map true_map false_map
   | While ->
   let end_map = linear_check_stmt list;
   merge_map lin_map end_map
   in
   let new_map, linear_result = List.fold_left linear_check_stmt body lin_map;
   merge_map lin_map new_map
   Ok lin_map

   let rec linear_check_expr () : linear_map result
   (* TODO implement *)
   Ok lin_map

   let linear_check_return (lin_map : linear_map) (sfunc_def.sret) : linear_map result =

   let rec linear_check_func (lin_map : linear_map) (func : sfunc_def) : linear_map result =
   linear_add_args lin_map func.sargs >>= fun lin_map ->
   linear_add_locals lin_map func.slocals >>= fun lin_map ->
   linear_check_stmt_list lin_map func.sbody >>= fun lin_map ->
   linear_check_return lin_map func.sret >>= fun lin_map

   in *)
*)

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
  let acc_linear_func (acc : func_info) (func : sfunc_def) : func_info =
    if func_has_linear_args func || func_has_linear_ret func
    then StringMap.add func.sfname func acc
    else acc
  in
  List.fold_left acc_linear_func StringMap.empty funcs
;;

(* Check linearity on a program *)
let check program : linear_result =
  let _, structs, funcs = program in
  (* Generate struct info map *)
  let struct_info_map = process_structs structs in
  (* populate func map *)
  let func_info_map =
    Result.to_option (Result.map (gen_func_info_map funcs) struct_info_map)
  in
  (* check functions *)
  struct_info_map, func_info_map, Ok StringMap.empty
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
