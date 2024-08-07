(* Abstract Syntax Tree for Crusty *)

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
  | AccessOp of string * accessOp * string
  | Deref of string
  | Borrow of string

(* Assignment Types *)
and assignment =
  | Assign of expr * expr
  | DerefAssign of string * expr
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

type return_stmt =
  | Return of expr
  | VoidReturn

(* Statement Types *)
type stmt =
  | Block of (var_decl list) * (stmt list)
  | FBlock of (var_decl list) * (stmt list) * return_stmt
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Break
  | Continue

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
  ; structs : struct_def list
  ; funcs : func_def list
  }
