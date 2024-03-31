(* First Draft Abstract Syntax Tree  for Crusty *)
(* TODO arrays and strings *)

(* Type Types *)
type typ =
  | Int
  | Bool
  | Char
  | Float
  | Struct of string

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
  | StringLit of string

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
type var_decl =  linear_qual * typ * string

type const_qualified_var_decl = 
  | Const of var_decl
  | Var of var_decl


type arg_decl = ref_qual * var_decl

(* Function Definition Type *)
type func_def =
  { rtyp : typ
  ; fname : string
  ; args : arg_decl list
  ; locals : const_qualified_var_decl list
  ; body : stmt list
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


  (* AST Pretty Printer *)
  let rec string_of_typ = function
    | Int -> "int"
    | Bool -> "bool"
    | Char -> "char"
    | Float -> "float"
    | Struct s -> "struct " ^ s

  let rec string_of_binArithOp = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"

  let rec string_of_binLogOp = function
    | And -> "&&"
    | Or -> "||"


    let rec string_of_unLogOp : unLogOp -> string = function
      | Not -> "!"

(* TODO how to differentiate pre/post *)

  let rec string_of_compOp = function
    | Eq -> "=="
    | Neq -> "!="
    | Lt -> "<"
    | Gt -> ">"
    | Leq -> "<="
    | Geq -> ">="

  let rec string_of_unArithOp = function
    | Neg -> "-"
    | Not -> "!"
    | PreInc -> "++"
    | PreDec -> "--"
    | PostInc -> "++"
    | PostDec -> "--"

  let rec string_of_accessOp = function
    | Dot -> "."
    | Arrow -> "->"

  let rec string_of_literal = function
    | IntLit i -> string_of_int i
    | BoolLit b -> string_of_bool b
    | CharLit c -> "'" ^ String.make 1 c ^ "'"
    | FloatLit f -> string_of_float f
    | StructLit exprs -> "{" ^ String.concat ", " (List.map string_of_expr exprs) ^ "}"
    | StringLit s -> "\"" ^ s ^ "\""

  and string_of_expr = function
    | Id s -> s
    | Literal lit -> string_of_literal lit
    | Operation op -> string_of_operation op
    | Assignment assign -> string_of_assignment assign
    | Call (fname, args) -> fname ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"

  and string_of_operation = function
    | ArithOp (e1, op, e2) -> string_of_expr e1 ^ " " ^ string_of_binArithOp op ^ " " ^ string_of_expr e2
    | UnArithOp (op, e) -> string_of_unArithOp op ^ string_of_expr e
    | CompOp (e1, op, e2) -> string_of_expr e1 ^ " " ^ string_of_compOp op ^ " " ^ string_of_expr e2
    | LogOp (e1, op, e2) -> string_of_expr e1 ^ " " ^ string_of_binLogOp op ^ " " ^ string_of_expr e2
    | UnLogOp (op, e) -> string_of_unLogOp op ^ string_of_expr e
    | AccessOp (e, op, s) -> string_of_expr e ^ string_of_accessOp op ^ s
    | Deref e -> "*" ^ string_of_expr e
    | Borrow e -> "&" ^ string_of_expr e

  and string_of_assignment = function
    | Assign (s, e) -> s ^ " = " ^ string_of_expr e
    | StructAssign (s1, s2, e) -> s1 ^ "." ^ s2 ^ " = " ^ string_of_expr e
    | RefStructAssign (s1, s2, e) -> s1 ^ "->" ^ s2 ^ " = " ^ string_of_expr e
    | StructExplode (fields, e) -> "{" ^ String.concat ", " fields ^ "} = " ^ string_of_expr e

  let rec string_of_stmt = function
    | Block stmts -> "{\n" ^ String.concat ";\n" (List.map string_of_stmt stmts) ^ ";\n}"
    | Expr e -> string_of_expr e ^ ";"
    | If (cond, stmt1, stmt2) -> "if (" ^ string_of_expr cond ^ ")\n" ^ string_of_stmt stmt1 ^ "\nelse\n" ^ string_of_stmt stmt2
    | While (cond, stmt) -> "while (" ^ string_of_expr cond ^ ")\n" ^ string_of_stmt stmt
    | Break -> "break;"
    | Continue -> "continue;"
    | Return e -> "return " ^ string_of_expr e ^ ";"

  let rec string_of_var_decl (qual, typ, name) =
    let qual_str = match qual with
      | Unrestricted -> ""
      | Linear -> "linear "
    in
    qual_str ^ string_of_typ typ ^ " " ^ name

  let rec string_of_const_qualified_var_decl = function
    | Const var_decl -> "const " ^ string_of_var_decl var_decl
    | Var var_decl -> string_of_var_decl var_decl

  let rec string_of_arg_decl (qual, var_decl) =
    let qual_str = match qual with
      | Ref -> "ref "
      | Val -> ""
    in
    qual_str ^ string_of_var_decl var_decl

  let string_of_func_def func_def =
    let rtyp_str = string_of_typ func_def.rtyp in
    let args_str = String.concat ", " (List.map string_of_arg_decl func_def.args) in
    let locals_str = String.concat ";\n" (List.map string_of_const_qualified_var_decl func_def.locals) in
    let body_str = String.concat ";\n" (List.map string_of_stmt func_def.body) in
    rtyp_str ^ " " ^ func_def.fname ^ "(" ^ args_str ^ ") {\n" ^ locals_str ^ ";\n" ^ body_str ^ ";\n}"

  let string_of_struct_def struct_def =
    let fields_str = String.concat ";\n" (List.map string_of_const_qualified_var_decl struct_def.fields) in
    "struct " ^ struct_def.sname ^ " {\n" ^ fields_str ^ ";\n}"

  let string_of_program program =
    let globals_str = String.concat ";\n" (List.map string_of_const_qualified_var_decl program.globals) in
    let structs_str = String.concat ";\n" (List.map string_of_struct_def program.structs) in
    let funcs_str = String.concat ";\n" (List.map string_of_func_def program.funcs) in
    globals_str ^ ";\n" ^ structs_str ^ ";\n" ^ funcs_str
