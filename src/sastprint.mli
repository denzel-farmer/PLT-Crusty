val string_of_prim_typ : Ast.primType -> string
val string_of_lin : Ast.lin_qual -> string
val string_of_typ : Ast.typ -> string
val string_of_ret_typ : Ast.ret_typ -> string
val string_of_binArithOp : Ast.binArithOp -> string
val string_of_binLogOp : Ast.binLogOp -> string
val string_of_unLogOp : Ast.unLogOp -> string
val string_of_compOp : Ast.compOp -> string
val string_of_unArithOp : Ast.unArithOp -> string
val string_of_accessOp : Ast.accessOp -> string
val string_of_literal : Sast.sliteral -> string
val string_of_sexpr : Sast.sexpr -> string
val string_of_sexpr_list : Sast.sexpr list -> string
val string_of_soperation : Sast.soperation -> string
val string_of_sassignment : Sast.sassignment -> string
val string_of_var_decl : Ast.typ * string -> string
val string_of_sstmt : Sast.sstmt -> string
val string_of_sreturn : Sast.sreturn_stmt -> string
val string_of_sfunc_prototype :
  Ast.ret_typ -> string -> (Ast.typ * string) list -> string
val string_of_sfunc_def : Sast.sfunc_def -> string
val string_of_sstruct_def : Ast.struct_def -> string
val string_of_sprogram :
  (Ast.typ * string) list * Ast.struct_def list * Sast.sfunc_def list ->
  string
