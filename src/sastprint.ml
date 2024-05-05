open Ast
open Sast
open Astprint

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
let rec string_of_typ = function
  | Prim (lin, prim) -> string_of_lin lin ^ " " ^ string_of_prim_typ prim
  | Struct s -> "struct " ^ s
  | Arr (t, s) -> string_of_typ t ^ "[" ^ string_of_int s ^ "]"
  | Ref t -> "ref " ^ string_of_typ t

let string_of_ret_typ = function
  | Void -> "void"
  | Nonvoid t -> string_of_typ t

let string_of_binArithOp = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"

let string_of_binLogOp = function
  | And -> "&&"
  | Or -> "||"

let string_of_unLogOp : unLogOp -> string = function
  | Not -> "!"

let string_of_compOp = function
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Leq -> "<="
  | Geq -> ">="

let string_of_unArithOp = function
  | Neg -> "-"
  | PreInc -> "++"
  | PreDec -> "--"
  | PostInc -> "++"
  | PostDec -> "--"

let string_of_accessOp = function
  | Dot -> "."
  | Arrow -> "->"

let rec string_of_literal = function
  | SIntLit i -> string_of_int i
  | SBoolLit b -> string_of_bool b
  | SCharLit c -> "'" ^ String.make 1 c ^ "'"
  | SFloatLit f -> string_of_float f
  | SStructLit sexprs -> "{" ^ String.concat ", " (List.map string_of_sexpr sexprs) ^ "}"
  | SStringLit s -> "\"" ^ s ^ "\""
  | SArrayLit sexprs -> "[" ^ String.concat ", " (List.map string_of_sexpr sexprs) ^ "]"

and string_of_sexpr (typ, sx) =
  string_of_typ typ ^ " " ^ match sx with
  | SId s -> s
  | SLiteral lit -> string_of_literal lit
  | SOperation op -> string_of_soperation op
  | SAssignment assign -> string_of_sassignment assign
  | SCall (fname, args) ->
    fname ^ "(" ^ String.concat ", " (List.map string_of_sexpr args) ^ ")"

and string_of_soperation = function
  | SArithOp (e1, op, e2) ->
    string_of_sexpr e1 ^ " " ^ string_of_binArithOp op ^ " " ^ string_of_sexpr e2
  | SUnArithOp (op, e) -> string_of_unArithOp op ^ string_of_sexpr e
  | SCompOp (e1, op, e2) ->
    string_of_sexpr e1 ^ " " ^ string_of_compOp op ^ " " ^ string_of_sexpr e2
  | SLogOp (e1, op, e2) ->
    string_of_sexpr e1 ^ " " ^ string_of_binLogOp op ^ " " ^ string_of_sexpr e2
  | SUnLogOp (op, e) -> string_of_unLogOp op ^ string_of_sexpr e
  | SAccessOp (e, op, s) -> string_of_sexpr e ^ string_of_accessOp op ^ s
  | SDeref e -> "*" ^ string_of_sexpr e
  | SBorrow e -> "&" ^ string_of_sexpr e
  | SIndex (s, e2) -> s ^ "[" ^ string_of_sexpr e2 ^ "]"

and string_of_sassignment = function
  | SAssign (s, e) -> s ^ " = " ^ string_of_sexpr e
  | SStructAssign (s1, s2, e) -> s1 ^ "." ^ s2 ^ " = " ^ string_of_sexpr e
  | SRefStructAssign (s1, s2, e) -> s1 ^ "->" ^ s2 ^ " = " ^ string_of_sexpr e
  | SStructExplode (fields, e) ->
    ":{" ^ String.concat ", " fields ^ "} = " ^ string_of_sexpr e

let rec string_of_sstmt = function
  | SBlock stmts -> "{\n" ^ String.concat "\n" (List.map string_of_sstmt stmts) ^ "\n}"
  | SExpr e -> string_of_sexpr e 
  | SIf (cond, stmt1, stmt2) ->
    "if ("
    ^ string_of_sexpr cond
    ^ ")\n"
    ^ string_of_sstmt stmt1
    ^ "\nelse\n"
    ^ string_of_sstmt stmt2
  | SWhile (cond, stmt) -> "while (" ^ string_of_sexpr cond ^ ")\n" ^ string_of_sstmt stmt
  | SBreak -> "break;"
  | SContinue -> "continue;"

let string_of_sreturn = function 
  | SReturn e -> "return " ^ string_of_sexpr e
  | SVoidReturn -> "return"

let string_of_sfunc_def func_def =
  let rtyp_str = string_of_ret_typ func_def.srtyp in
  let args_str = String.concat ", " (List.map string_of_var_decl func_def.sargs) in
  let locals_str =
    String.concat ";\n" (List.map string_of_var_decl func_def.slocals)
  in
  let body_str = String.concat ";\n" (List.map string_of_sstmt func_def.sbody) in
  let return_str = string_of_sreturn func_def.sreturn in
  rtyp_str
  ^ " "
  ^ func_def.sfname
  ^ "("
  ^ args_str
  ^ ") {\n"
  ^ locals_str
  ^ ";\n"
  ^ body_str
  ^ ";\n"
  ^ return_str
  ^ ";\n}"

let string_of_sstruct_def struct_def =
  let fields_str =
    String.concat ";\n" (List.map string_of_var_decl struct_def.sfields)
  in
  string_of_lin struct_def.lin_qual ^ " struct " ^ struct_def.sname ^ " {\n" ^ fields_str ^ ";\n}"

let string_of_sprogram (sglobals, sstructs, sfuncs) =
  let globals_str =
    String.concat ";\n" (List.map string_of_var_decl sglobals)
  in
  let structs_str = String.concat ";\n" (List.map string_of_sstruct_def sstructs) in
  let funcs_str = String.concat "" (List.map string_of_sfunc_def sfuncs) in
  let globals_end = if String.length globals_str > 0 then ";\n" else "" in
  let structs_end = if String.length structs_str > 0 then ";\n" else "" in
  globals_str ^ globals_end ^ structs_str ^ structs_end ^ funcs_str
