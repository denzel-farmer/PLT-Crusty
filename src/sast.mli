open Ast

type sexpr = typ * expr_detail 
and expr_detail =   
      SId of string
    | SLiteral of literal
    | SOperation of operation
    | SAssignment of assignment
    | SCall of string * sexpr list

(* Statement Types *)
type sstmt =
  | SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  (* | Break
  | Continue *)

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
type sprogram =
  { sglobals : const_qualified_var_decl list
  ; sstructs : sstruct_def list
  ; sfuncs : sfunc_def list
  }


(* Declaration Types *)
type var_decl = linear_qual * typ * string

(* type array_decl = string * int

type const_array_decl = 
  | Arr of var_decl *)

type const_qualified_var_decl =
  | Const of var_decl
  | Var of var_decl

type arg_decl = ref_qual * var_decl

