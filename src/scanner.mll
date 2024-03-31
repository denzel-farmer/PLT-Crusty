(* Ocamllex scanner for Crusty *)

{ open Crustyparse }


let whitespace = [' ' '\t' '\r' '\n']
let newline = ['\n']

let digit = ['0'-'9']
let int = digit+ | '-'digit+ | '+'digit+
let letter = ['a'-'z' 'A'-'Z']

let float_base = digit* '.' digit+ | digit+ '.' digit*
let float_exp = ['e' 'E'] ['+' '-']? digit+
let float = float_base float_exp? | digit+float_exp

let single_enclosed = "'" _ "'"
let double_enclosed = '"' _ '"'

rule token = parse
  whitespace { token lexbuf }
| "/*"     { comment lexbuf }
| "//"     { line_comment lexbuf }

(* Grouping/Separating *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
(*| ':'      { COLON } *)
| ','      { COMMA }
| ':'      { EXPLODE }

(* Keywords *)

(* Primitive Types *)
| "int"   { INT }
| "bool"  { BOOL }
| "char"  { CHAR }
| "float" { FLOAT }
| "void"  { VOID }


(* Literals *)
| int as lem  { INTLIT(int_of_string lem) }
| float as lem { FLOATLIT(float_of_string lem) }
| "true"   { BOOLLIT(true)  }
| "false"  { BOOLLIT(false) }
| single_enclosed as lem { CHARLIT(lem.[1]) } (* TODO ensure this includes special characters like /0 *)
| double_enclosed as lem { STRINGLIT(String.sub lem 1 ((String.length lem) - 2)) } (*TODO escape sequences *)

(* Type Qualifiers *)
| "ref"   { REF }
| "linear" { LINEAR }
| "unrestricted" { UNRESTRICTED }
| "const" { CONST }


(* Compount Types *)
| "struct" { STRUCT }

(* Operators *)
| '='      { ASSIGN }

(* Arithmetic Operators *)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MOD }

| "++"     { INCR }
| "--"     { DECR }

(* TODO Bitwise Operators *)

(* Comparison Operators*)
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LTE }
| '>'      { GT }
| ">="     { GTE }

(* Logical Operators *)
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }


(* Struct Operators *)
| '.'      { DOT }

(* Borrowing Operators *)
| '&'      { BORROW }
| '*'      { DEREF }
| "->"     { ARROW }

(* Control Flow *)
| "if"     { IF }
| "else"   { ELSE }

| "while"  { WHILE }
| "break"  { BREAK }
| "continue" { CONTINUE }

| "return" { RETURN }

(* Identifier *)
| letter (digit | letter | '_')* as lem { ID(lem) }

  (* End of File *)
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and line_comment = parse
  newline { token lexbuf }
  | _ { line_comment lexbuf }
