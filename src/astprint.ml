open Ast

let string_of_prim_typ = function
  | Int -> "int"
  | Bool -> "bool"
  | Char -> "char"
  | Float -> "float"
  | String -> "string"

let string_of_lin = function
  | Unrestricted -> "unrestricted"
  | Linear -> "linear"

(* AST Pretty Printer *)
let rec string_of_typ : typ -> string = function
  | Prim (lin, prim) -> string_of_lin lin ^ " " ^ string_of_prim_typ prim
  | Struct s -> "struct " ^ s
  | Arr (t, s) -> string_of_typ t ^ "[" ^ string_of_int s ^ "]"
  | Ref t -> "ref " ^ string_of_typ t
;;

let string_of_ret_typ = function
  | Void -> "void"
  | Nonvoid t -> string_of_typ t

let string_of_binArithOp = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
;;

let string_of_binLogOp = function
  | And -> "&&"
  | Or -> "||"
;;

let string_of_unLogOp : unLogOp -> string = function
  | Not -> "!"
;;

(* TODO how to differentiate pre/post *)

let string_of_compOp = function
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Leq -> "<="
  | Geq -> ">="
;;

let string_of_unArithOp = function
  | Neg -> "-"
  | PreInc -> "++"
  | PreDec -> "--"
  | PostInc -> "++"
  | PostDec -> "--"
;;

let string_of_accessOp = function
  | Dot -> "."
  | Arrow -> "->"
;;

let rec string_of_literal = function
  | IntLit i -> string_of_int i
  | BoolLit b -> string_of_bool b
  | CharLit c -> "'" ^ String.make 1 c ^ "'"
  | FloatLit f -> string_of_float f
  | StructLit exprs -> "{" ^ String.concat ", " (List.map string_of_expr exprs) ^ "}"
  | StringLit s -> "\"" ^ s ^ "\""
  | ArrayLit exprs -> "[" ^ String.concat ", " (List.map string_of_expr exprs) ^ "]"

and string_of_expr = function
  | Id s -> s
  | Literal lit -> string_of_literal lit
  | Operation op -> string_of_operation op
  | Assignment assign -> string_of_assignment assign
  | Call (fname, args) ->
    fname ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"

and string_of_operation = function
  | ArithOp (e1, op, e2) ->
    string_of_expr e1 ^ " " ^ string_of_binArithOp op ^ " " ^ string_of_expr e2
  | UnArithOp (op, e) -> string_of_unArithOp op ^ string_of_expr e
  | CompOp (e1, op, e2) ->
    string_of_expr e1 ^ " " ^ string_of_compOp op ^ " " ^ string_of_expr e2
  | LogOp (e1, op, e2) ->
    string_of_expr e1 ^ " " ^ string_of_binLogOp op ^ " " ^ string_of_expr e2
  | UnLogOp (op, e) -> string_of_unLogOp op ^ string_of_expr e
  | AccessOp (e, op, s) -> string_of_expr e ^ string_of_accessOp op ^ s
  | Deref s -> "*" ^ s
  | Borrow s -> "&" ^ s
  | Index (s, e2) -> s ^ "[" ^ string_of_expr e2 ^ "]"

and string_of_assignment = function
  | Assign (s, e) -> s ^ " = " ^ string_of_expr e
  | StructAssign (s1, s2, e) -> s1 ^ "." ^ s2 ^ " = " ^ string_of_expr e
  | RefStructAssign (s1, s2, e) -> s1 ^ "->" ^ s2 ^ " = " ^ string_of_expr e
  | StructExplode (fields, e) ->
    ":{" ^ String.concat ", " fields ^ "} = " ^ string_of_expr e
;;

let string_of_var_decl (typ, name) = 
  string_of_typ typ ^ " " ^ name

let rec string_of_stmt = function
  | Block (vars, stmts) -> "{\n" ^ String.concat "\n" (List.map string_of_var_decl vars) ^ String.concat "\n" (List.map string_of_stmt stmts) ^ "\n}"
  | Expr e -> string_of_expr e 
  | If (cond, stmt1, stmt2) ->
    "if ("
    ^ string_of_expr cond
    ^ ")\n"
    ^ string_of_stmt stmt1
    ^ "\nelse\n"
    ^ string_of_stmt stmt2
  | While (cond, stmt) -> "while (" ^ string_of_expr cond ^ ")\n" ^ string_of_stmt stmt
  | Break -> "break;"
  | Continue -> "continue;"
  (* | Return e -> "return " ^ string_of_expr e *)
;;

(* let string_of_var_decl (qual, typ, name) =
  let qual_str =
    match qual with
    | Unrestricted -> ""
    | Linear -> "linear "
  in
  qual_str ^ string_of_typ typ ^ " " ^ name
;; *)
(* 
let string_of_const_qualified_var_decl = function
  | Const var_decl -> "const " ^ string_of_var_decl var_decl
  | Var var_decl -> string_of_var_decl var_decl
;; *)

(* let string_of_arg_decl (qual, var_decl) =
  let qual_str =
    match qual with
    | Ref -> "ref "
    | Val -> ""
  in
  qual_str ^ string_of_var_decl var_decl
;; *)

let string_of_return = function 
  | Return e -> "return " ^ string_of_expr e
  | VoidReturn -> "return"

let string_of_func_def func_def =
  let rtyp_str = string_of_ret_typ func_def.rtyp in
  let args_str = String.concat ", " (List.map string_of_var_decl func_def.args) in
  let locals_str =
    String.concat ";\n" (List.map string_of_var_decl func_def.locals)
  in
  let body_str = String.concat ";\n" (List.map string_of_stmt func_def.body) in
  let return_str = string_of_return func_def.return in
  rtyp_str
  ^ " "
  ^ func_def.fname
  ^ "("
  ^ args_str
  ^ ") {\n"
  ^ locals_str
  ^ ";\n"
  ^ body_str
  ^ ";\n"
  ^ return_str
  ^ ";\n}"
;;

let string_of_struct_def struct_def =
  let fields_str =
    String.concat ";\n" (List.map string_of_var_decl struct_def.fields)
  in
  string_of_lin struct_def.lin_qual ^ " struct " ^ struct_def.sname ^ " {\n" ^ fields_str ^ ";\n}"
;;

let string_of_program program =
  let globals_str =
    String.concat ";\n" (List.map string_of_var_decl program.globals)
  in
  let structs_str = String.concat ";\n" (List.map string_of_struct_def program.structs) in
  let funcs_str = String.concat "" (List.map string_of_func_def program.funcs) in
  let globals_end = if String.length globals_str > 0 then ";\n" else "" in
  let structs_end = if String.length structs_str > 0 then ";\n" else "" in
  globals_str ^ globals_end ^ structs_str ^ structs_end ^ funcs_str
;;
