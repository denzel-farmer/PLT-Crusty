(* Ocamllex scanner for Crusty *)

{ open Crustyparse
  let printf = Format.eprintf
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

rule token = parse
  whitespace { token lexbuf }
| "/*"     { comment lexbuf }
| "//"     { line_comment lexbuf }

(* Grouping/Separating *)
| '('      { printf "LPAREN "; LPAREN }
| ')'      { printf "RPAREN "; RPAREN }
| '{'      { printf "LBRACE "; LBRACE }
| '}'      { printf "RBRACE "; RBRACE }
| '['      { printf "LBRACK "; LBRACK }
| ']'      { printf "RBRACK "; RBRACK }
| ';'      { printf "SEMI "; SEMI }
| ','      { printf "COMMA "; COMMA }
| ':'      { printf "EXPLODE "; EXPLODE }

(* Keywords *)

(* Primitive Types *)
| "int"   { printf "INT "; INT }
| "bool"  { printf "BOOL "; BOOL }
| "char"  { printf "CHAR "; CHAR }
| "float" { printf "FLOAT "; FLOAT }
| "void"  { printf "VOID "; VOID }
| "string" { printf "STRING "; STRING }

(* Literals *)
| int as lem  { printf "%s " ("INTLIT(" ^ lem ^ ")"); INTLIT(int_of_string lem) }
| float as lem { printf "%s " ("FLOATLIT(" ^ lem ^ ")"); FLOATLIT(float_of_string lem) }
| "true"   { printf "BOOLLIT(true) "; BOOLLIT(true)  }
| "false"  { printf "BOOLLIT(false) "; BOOLLIT(false) }
| single_enclosed as lem { printf "%s " ("CHARLIT(" ^ lem ^ ")"); CHARLIT(lem.[1]) }
(* TODO this doesn't actually work--need more complex regex *)
| double_enclosed as lem { printf "%s " ("CHARLIT(" ^ lem ^ ")"); STRINGLIT(String.sub lem 1 ((String.length lem) - 2)) }

(* Type Qualifiers *)
| "ref"   { printf "REF "; REF }
| "linear" { printf "LINEAR "; LINEAR }
| "unrestricted" { printf "UNRESTRICTED "; UNRESTRICTED }
| "const" { printf "CONST "; CONST }
(* Compound Types *)
| "struct" { printf "STRUCT "; STRUCT }

(* Operators *)
| '='      { printf "ASSIGN "; ASSIGN }

(* Arithmetic Operators *)
| '+'      { printf "PLUS "; PLUS }
| '-'      { printf "MINUS "; MINUS }
| '*'      { printf "STAR "; STAR }
| '/'      { printf "DIVIDE "; DIVIDE }
| '%'      { printf "MOD "; MOD }
| "++"     { printf "INCR "; INCR }
| "--"     { printf "DECR "; DECR }

(* TODO Bitwise Operators *)

(* Comparison Operators*)
| "=="     { printf "EQ "; EQ }
| "!="     { printf "NEQ "; NEQ }
| '<'      { printf "LT "; LT }
| "<="     { printf "LTE "; LTE }
| '>'      { printf "GT "; GT }
| ">="     { printf "GTE "; GTE }

(* Logical Operators *)
| "&&"     { printf "AND "; AND }
| "||"     { printf "OR "; OR }
| "!"      { printf "NOT "; NOT }

(* Struct Operators *)
| '.'      { printf "DOT "; DOT }

(* Borrowing Operators *)
| '&'      { printf "BORROW "; BORROW }
(* | '*'      { printf " DEREF"; DEREF } *)
| "->"     { printf "ARROW "; ARROW }

(* Control Flow *)
| "if"     { printf "IF "; IF }
| "else"   { printf "ELSE "; ELSE }
| "while"  { printf "WHILE "; WHILE }
| "break"  { printf "BREAK "; BREAK }
| "continue" { printf "CONTINUE "; CONTINUE }
| "return" { printf "RETURN "; RETURN }

(* Identifier *)
| letter (digit | letter | '_')* as lem { printf "%s " ("ID(" ^ lem ^ ")"); ID(lem) }

(* End of File *)
| eof { printf "EOF "; EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and line_comment = parse
  newline { token lexbuf }
| _ { line_comment lexbuf }
