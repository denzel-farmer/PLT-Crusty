type log_level = Debug | Info
val log_print : string -> log_level -> unit
val log_println : string -> log_level -> unit
val debug_print : string -> unit
val debug_println : string -> unit
val info_print : string -> unit
val info_println : string -> unit
