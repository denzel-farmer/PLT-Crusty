(* First Draft Abstract Syntax Tree  for Crusty *)
(* TODO arrays and strings *)

(* Type Types *)
type typ =
  | Int
  | Bool
  | Char
  | Float
  | Struct of string

(* TODO unify typ and ret_typ*)
type ret_typ =
  | Int
  | Bool
  | Char
  | Float
  | Struct of string
  | Void

(* Operator Types *)
type binArithOp =
  | Add
  | Sub
  | Mult
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
  | Not
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
  | Deref of expr
  | Borrow of expr

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
  | StructLit of expr list

(* Statement Types *)
type stmt =
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Break
  | Continue
  | Return of expr

(* Type Qualifiers *)
type const_qual =
  | Const
  | Var

type linear_qual =
  | Unrestricted
  | Linear

type ref_qual =
  | Ref
  | Val

(* Declaration Types *)
type var_decl = const_qual * linear_qual * typ * string
type arg_decl = ref_qual * var_decl

(* Function Definition Type *)
type func_def =
  { rtyp : typ
  ; fname : string
  ; args : arg_decl list
  ; locals : var_decl list
  ; body : stmt list
  }

(* Struct Definition Type *)
type struct_def =
  { sname : string
  ; fields : var_decl list
  }

(* Program Type *)
type program =
  { globals : var_decl list
  ; structs : struct_def list
  ; funcs : func_def list
  }
