type log_level =
  | Debug
  | Info

(* Print functions for debugging and error messages *)
let log_level = Some Debug

let log_print (msg : string) (level : log_level) : unit =
  match log_level, level with
  | Some Debug, Debug | Some Debug, Info -> output_string Pervasives.stderr msg
  | Some Info, Info -> output_string Pervasives.stderr msg
  | Some Info, Debug -> ()
  | None, _ -> ()
;;

let log_println (msg : string) (level : log_level) : unit = log_print (msg ^ "\n") level
let debug_print (msg : string) : unit = log_print msg Debug
let debug_println (msg : string) : unit = log_println ("[DEBUG] " ^ msg) Debug
let info_print (msg : string) : unit = log_print msg Info
let info_println (msg : string) : unit = log_println ("[INFO] " ^ msg) Info
