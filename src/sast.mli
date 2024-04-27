open Ast

type sexpr = typ * expr 
and expr =   
      SId of string
    | SLiteral of literal
    | SOperation of operation
    | SAssignment of assignment
    | SCall of string * sexpr list

(* Operation Types *)
and operation =
    | SArithOp of sexpr * binArithOp * sexpr
    | SUnArithOp of unArithOp * sexpr
    | SCompOp of sexpr * compOp * sexpr
    | SLogOp of sexpr * binLogOp * sexpr
    | SUnLogOp of unLogOp * sexpr
    | SAccessOp of sexpr * accessOp * string
    | SDeref of sexpr
    | SBorrow of sexpr
  
(* Assignment Types *)
and assignment =
    | SAssign of string * sexpr
    | SStructAssign of string * string * sexpr
    | SRefStructAssign of string * string * sexpr
    | SStructExplode of string list * sexpr

and literal =
  | SIntLit of int
  | SBoolLit of bool
  | SCharLit of char
  | SFloatLit of float
  | SStructLit of sexpr list
  | SStringLit of string
  | SArrayLit of sexpr list

(* Statement Types *)
type sstmt =
  | SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  | SBreak
  | SContinue

(* Function Definition Type *)
type sfunc_def =
  { srtyp : typ
  ; sfname : string
  ; sargs : arg_decl list
  ; slocals : const_qualified_var_decl list
  ; sbody : sstmt list
  ; sreturn: return_stmt
  }

(* Struct Definition Type *)
type sstruct_def =
{ sname : string
; sfields : const_qualified_var_decl list
}

(* Program Type *)
type program =
  { sglobals : const_qualified_var_decl list
  ; sstructs : sstruct_def list
  ; sfuncs : sfunc_def list
  }

