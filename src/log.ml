type log_level =
  | Debug
  | Info

(* Print functions for debugging and error messages *)
let log_level = (Some Debug)
(* let set_log_level (new_level : log_level) : unit = log_level := new_level
let set_log_level_debug = set_log_level (Some Debug)
let set_log_level_info = set_log_level (Some Info)
let disable_logging = set_log_level None *)

let log_print (msg : string) (level : log_level) : unit =
  match log_level, level with
  | Some Debug, Debug | Some Debug, Info -> output_string Stdlib.stderr msg
  | Some Info, Info -> output_string Stdlib.stderr msg
  | Some Info, Debug -> ()
  | None, _ -> ()
;;

let log_println (msg : string) (level : log_level) : unit = log_print (msg ^ "\n") level
let debug_print (msg : string) : unit = log_print msg Debug
let debug_println (msg : string) : unit = log_println ("[DEBUG] " ^ msg) Debug
let info_print (msg : string) : unit = log_print msg Info
let info_println (msg : string) : unit = log_println ("[INFO] " ^ msg) Info
