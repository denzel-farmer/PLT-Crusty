open Sast 


type linear_result = 
  | LinearError of string
  | LinPass


type linear_state = 
  | Unassigned 
  | Assigned 
  | Used


(* Hash table that maps name to linearity and type information *)
type linear_map = (string, (linear_state * stype)) Hashtbl.t

let linear_add_args (lin_map : linear_map) (args : sfunc_def.sargs) : linear_result =
  
in 

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
        let lin_map, linear_result = linear_check_stmt lin_map func.sbody in 

        if (linear_result = Pass) then 

          (* Check linearity on the return type *)
          let lin_map, linear_result = linear_check_expr lin_map func.sret in 

          if (linear_result = Pass) then LinPass
          else lin_map, linear_result
        else lin_map, linear_result
      else lin_map, linear_result
  else lin_map, linear_result 



  (* Check linearity on each statement in the function body, passing in lin_map *)

  (* Do a check on the return type? *)

in 

(* Check linearity on a program *)
let rec linear_check (program : sprogram): linear_result = LinPass
  (* Initialize the linear map object *)
  let lin_map = Hashtbl.create 0 in 

  (* Call linearity checking on each function, passing linear vars object to each (probably not needed) *)
  let func_results = List.map linear_check_func lin_map program.sfuncs in

  (* If any function fails linearity checking, return the first error, otherwise pass *)
  if List.exists (fun x -> x = LinearError) func_results then LinearError "Function failed linearity check"
  else Pass


in 



let rec linear_check_stmt () : 

in 


let rec linear_check_expr () :







