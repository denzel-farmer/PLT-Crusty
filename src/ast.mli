(* First Draft Abstract Syntax Tree  for Crusty *)
(* TODO arrays and strings *)

(* Type Types *)
type typ =
  | Int
  | Bool
  | Char
  | Float
  | Struct of string
  | Arr of typ * int

(* TODO add VOID function return type *)

(* Operator Types *)
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
  | StringLit of string
  | ArrayLit of expr list

(* Statement Types *)
type stmt =
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Break
  | Continue

(* Type Qualifiers *)
(* type const_qual =
  | Const
  | Var  *)

type linear_qual =
  | Unrestricted
  | Linear

type ref_qual =
  | Ref
  | Val

(* Declaration Types *)
type var_decl = linear_qual * typ * string

(* type array_decl = string * int

type const_array_decl = 
  | Arr of var_decl *)

type const_qualified_var_decl =
  | Const of var_decl
  | Var of var_decl

type arg_decl = ref_qual * var_decl

type return_stmt = 
  | Return of expr

(* Function Definition Type *)
type func_def =
  { rtyp : typ
  ; fname : string
  ; args : arg_decl list
  ; locals : const_qualified_var_decl list
  ; body : stmt list
  ; return: return_stmt
  }

(* Struct Definition Type *)
type struct_def =
  { sname : string
  ; fields : const_qualified_var_decl list
  }

(* Program Type *)
type program =
  { globals : const_qualified_var_decl list
  ; structs : struct_def list
  ; funcs : func_def list
  }