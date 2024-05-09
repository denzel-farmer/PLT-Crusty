open Ast

(* typ includes reference and linear qualifier if primative *)
type sexpr = typ * sx

and sx =
  | SId of string
  | SLiteral of sliteral
  | SOperation of soperation
  | SAssignment of sassignment
  | SCall of string * sexpr list

(* Operation Types *)
and soperation =
  | SArithOp of sexpr * binArithOp * sexpr
  | SUnArithOp of unArithOp * sexpr
  | SCompOp of sexpr * compOp * sexpr
  | SLogOp of sexpr * binLogOp * sexpr
  | SUnLogOp of unLogOp * sexpr
  | SAccessOp of sexpr * accessOp * string
  | SDeref of string
  | SBorrow of string
  | SIndex of string * sexpr

(* Assignment Types *)
and sassignment =
  | SAssign of string * sexpr
  | SStructAssign of string * string * sexpr
  | SRefStructAssign of string * string * sexpr
  | SStructExplode of string list * sexpr

and sliteral =
  | SIntLit of int
  | SBoolLit of bool
  | SCharLit of char
  | SFloatLit of float
  | SStructLit of sexpr list
  | SStringLit of string
  | SArrayLit of sexpr list

(* Statement Types *)
type sstmt =
  | SBlock of (var_decl list) * (sstmt list)
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  | SBreak
  | SContinue

type sreturn_stmt =
  | SReturn of sexpr
  | SVoidReturn

(* Argument Declaration Type *)

(* Function Definition Type *)
type sfunc_def =
  { srtyp : ret_typ
  ; sfname : string
  ; sargs : var_decl list
  ; slocals : var_decl list
  ; sbody : sstmt list
  ; sreturn : sreturn_stmt
  }

(* Struct Definition Type *)
type sstruct_def =
  { lin_qual : lin_qual
  ; sname : string
  ; sfields : var_decl list
  }

(* Program Type *)
type sprogram =
  { sglobals : var_decl list
  ; sstructs : struct_def list
  ; sfuncs : sfunc_def list
  }
