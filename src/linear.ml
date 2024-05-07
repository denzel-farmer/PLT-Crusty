open Ast
open Sast 
open Result


(* TODO remove this and just use regular Error type
type linear_result = 
  | LinearError of string
  | LinPass *)

(* type linear_state = 
  | Unassigned 
  | Assigned
  | Borrowed
  | Used *)

(* Hash table that maps struct name to struct definition *)
type struct_map = (string, struct_def) Hashtbl.t
(* let string_of_linear_result = function
  | LinearError s -> s
  | LinPass -> "Pass"
;; *)
(* Hash table that maps name to linearity and type information *)
(* type linear_map = (string, (ref_qual * linear_state * stype)) Hashtbl.t *)

(* 
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


let rec linear_add_locals (lin_map : linear_map) (slocals : sfunc_def.slocals) : linear_map result = 
  (*TODO implement *)
  Ok lin_map

let rec linear_check_stmt_list (lin_map : linear_map) (sfunc_def.sbody) : linear_map result = 
  (* TODO implement *)
  Ok lin_map

let rec linear_check_expr ()
  (* TODO implement *)
  Ok lin_map


let rec linear_check_func (lin_map : linear_map) (func : sfunc_def) : linear_map result = 
  linear_add_args lin_map func.sargs >>= fun lin_map ->
  linear_add_locals lin_map func.slocals >>= fun lin_map ->
  linear_check_stmt_list lin_map func.sbody >>= fun lin_map ->
  linear_check_stmt_list lin_map func.sret >>= fun lin_map stmt or expr? *)


  (* match sstruct.lin_qual with
  | Unrestricted ->
    (* check that sstruct.sfields doesn't have linear types else LinError *)
  | Linear ->
    (* add structdef to lin_map, then check sstruct.sfields for any linear types *)
    Hashtbl.add lin_map arg_name (_, Assigned, typ); *)

  (* let linear_check_structs (structs : struct_def list) : struct_map result = 
    
    let linear_check_struct (last_struct_map : struct_map result) (curr_struct : struct_def) : struct_map result =
      (* check lin_qual of struct *)
      match last_struct_map with 
      | Error _ as e -> e
      | Ok _ as struct_map -> Ok (Hashtbl.add struct_map curr_struct.sname curr_struct)
    in 

    List.fold_left linear_check_struct (Hashtbl.create 0) structs
  in
 *)





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
  else lin_map, linear_result  *)



  (* Check linearity on each statement in the function body, passing in lin_map *)

  (* Do a check on the return type? *)


let check (program) : (struct_map, string) result =
  Ok (Hashtbl.create 0)
  ;

(* Check linearity on a program *)
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