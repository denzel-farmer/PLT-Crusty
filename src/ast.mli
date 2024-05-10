(* Abstract Syntax Tree for Crusty *)
(* TODO strings *)

(* Type Types *)
type lin_qual =
  | Unrestricted
  | Linear

type primType =
  | Int
  | Bool
  | Char
  | Float
  | String

type typ =
  | Prim of lin_qual * primType
  | Struct of string
  | Arr of typ * int
  | Ref of typ

type ret_typ =
  | Nonvoid of typ
  | Void

(* Operations *)

(* TODO add string operations? concat, split, etc. (probably too much work)*)
(* TODO add array operations? index (have it), concat, slice *)
(* Could alternatively make strings into arrays *)

type binArithOp =
  | Add
  | Sub
  | Mul
  | Div
  | Mod

type binLogOp =
  | And
  | Or

type unLogOp = Not

type compOp =
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq

type unArithOp =
  | Neg
  | PreInc
  | PreDec
  | PostInc
  | PostDec

type accessOp =
  | Dot
  | Arrow

(* Expression Type *)
type expr =
  | Id of string
  | Literal of literal
  | Operation of operation
  | Assignment of assignment
  | Call of string * expr list

(* Operation Types *)
and operation =
  | ArithOp of expr * binArithOp * expr
  | UnArithOp of unArithOp * expr
  | CompOp of expr * compOp * expr
  | LogOp of expr * binLogOp * expr
  | UnLogOp of unLogOp * expr
  | AccessOp of expr * accessOp * string
  | Deref of string
  | Borrow of string
  | Index of string * expr

(* Assignment Types *)
and assignment =
  | Assign of string * expr
  | StructAssign of string * string * expr
  | RefStructAssign of string * string * expr
  | StructExplode of string list * expr

(* Literal Types *)
and literal =
  | IntLit of int
  | BoolLit of bool
  | CharLit of char
  | FloatLit of float
  | StructLit of string * expr list
  | StringLit of string
  | ArrayLit of expr list

(* Declaration Types *)
type var_decl = typ * string

(* Statement Types *)
type stmt =
  | Block of (var_decl list) * (stmt list)
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Break
  | Continue

(*TODO re add const?*)

type return_stmt =
  | Return of expr
  | VoidReturn

(* Function Definition Type *)
type func_def =
  { rtyp : ret_typ
  ; fname : string
  ; args : var_decl list
  ; locals : var_decl list
  ; body : stmt list
  ; return : return_stmt
  }

(* Struct Definition Type *)
type struct_def =
  { lin_qual : lin_qual
  ; sname : string
  ; fields : var_decl list
  }

(* Program Type *)
type program =
  { globals : var_decl list
  ; structs : struct_def list (*TODO replace struct_def list with string map*)
  ; funcs : func_def list
  }
