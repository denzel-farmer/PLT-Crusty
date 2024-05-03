(* Ocamllex scanner for Crusty *)

{ open Crustyparse 
}

let whitespace = [' ' '\t' '\r' '\n']
let newline = '\r' | '\n' | "\r\n"

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let int = ['+' '-']? digit+

let float_base = digit* '.' digit+ | digit+ '.' digit*
let float_exp = ['e' 'E'] ['+' '-']? digit+
let float = float_base float_exp? | digit+ float_exp

let single_enclosed = "'" _ "'"
let double_enclosed = '"' _ '"'
(* 
let Format.eprintf str = Format.eprintf str  *)

rule token = parse
  whitespace { token lexbuf }
| "/*"     { comment lexbuf }
| "//"     { line_comment lexbuf }

(* Grouping/Separating *)
| '('      { Format.eprintf " LPAREN"; LPAREN }
| ')'      { Format.eprintf " RPAREN"; RPAREN }
| '{'      { Format.eprintf " LBRACE"; LBRACE }
| '}'      { Format.eprintf " RBRACE"; RBRACE }
| '['      { Format.eprintf " LBRACK"; LBRACK }
| ']'      { Format.eprintf " RBRACK"; RBRACK }
| ';'      { Format.eprintf " SEMI"; SEMI }
| ','      { Format.eprintf " COMMA"; COMMA }
| ':'      { Format.eprintf " EXPLODE"; EXPLODE }

(* Keywords *)

(* Primitive Types *)
| "int"   { Format.eprintf " INT"; INT }
| "bool"  { Format.eprintf " BOOL"; BOOL }
| "char"  { Format.eprintf " CHAR"; CHAR }
| "float" { Format.eprintf " FLOAT"; FLOAT }
| "void"  { Format.eprintf " VOID"; VOID }
| "string" { Format.eprintf " STRING"; STRING }

(* Literals *)
| int as lem  { Format.eprintf " %s" ("INTLIT(" ^ lem ^ ")"); INTLIT(int_of_string lem) }
| float as lem { Format.eprintf " %s" ("FLOATLIT(" ^ lem ^ ")"); FLOATLIT(float_of_string lem) }
| "true"   { Format.eprintf " BOOLLIT(true)"; BOOLLIT(true)  }
| "false"  { Format.eprintf " BOOLLIT(false)"; BOOLLIT(false) }
| single_enclosed as lem { Format.eprintf " %s" ("CHARLIT(" ^ lem ^ ")"); CHARLIT(lem.[1]) }
(* TODO this doesn't actually work--need more complex regex *)
| double_enclosed as lem { Format.eprintf " %s" ("CHARLIT(" ^ lem ^ ")"); STRINGLIT(String.sub lem 1 ((String.length lem) - 2)) }

(* Type Qualifiers *)
| "ref"   { Format.eprintf " REF"; REF }
| "linear" { Format.eprintf " LINEAR"; LINEAR }
| "unrestricted" { Format.eprintf " UNRESTRICTED"; UNRESTRICTED }
| "const" { Format.eprintf " CONST"; CONST }
(* Compound Types *)
| "struct" { Format.eprintf " STRUCT"; STRUCT }

(* Operators *)
| '='      { Format.eprintf " ASSIGN"; ASSIGN }

(* Arithmetic Operators *)
| '+'      { Format.eprintf " PLUS"; PLUS }
| '-'      { Format.eprintf " MINUS"; MINUS }
| '*'      { Format.eprintf " STAR"; STAR }
| '/'      { Format.eprintf " DIVIDE"; DIVIDE }
| '%'      { Format.eprintf " MOD"; MOD }
| "++"     { Format.eprintf " INCR"; INCR }
| "--"     { Format.eprintf " DECR"; DECR }

(* TODO Bitwise Operators *)

(* Comparison Operators*)
| "=="     { Format.eprintf " EQ"; EQ }
| "!="     { Format.eprintf " NEQ"; NEQ }
| '<'      { Format.eprintf " LT"; LT }
| "<="     { Format.eprintf " LTE"; LTE }
| '>'      { Format.eprintf " GT"; GT }
| ">="     { Format.eprintf " GTE"; GTE }

(* Logical Operators *)
| "&&"     { Format.eprintf " AND"; AND }
| "||"     { Format.eprintf " OR"; OR }
| "!"      { Format.eprintf " NOT"; NOT }

(* Struct Operators *)
| '.'      { Format.eprintf " DOT"; DOT }

(* Borrowing Operators *)
| '&'      { Format.eprintf " BORROW"; BORROW }
(* | '*'      { Format.eprintf " DEREF"; DEREF } *)
| "->"     { Format.eprintf " ARROW"; ARROW }

(* Control Flow *)
| "if"     { Format.eprintf " IF"; IF }
| "else"   { Format.eprintf " ELSE"; ELSE }
| "while"  { Format.eprintf " WHILE"; WHILE }
| "break"  { Format.eprintf " BREAK"; BREAK }
| "continue" { Format.eprintf " CONTINUE"; CONTINUE }
| "return" { Format.eprintf " RETURN"; RETURN }

(* Identifier *)
| letter (digit | letter | '_')* as lem { Format.eprintf " %s" ("ID(" ^ lem ^ ")"); ID(lem) }

(* End of File *)
| eof { Format.eprintf " EOF"; EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and line_comment = parse
  newline { token lexbuf }
| _ { line_comment lexbuf }
