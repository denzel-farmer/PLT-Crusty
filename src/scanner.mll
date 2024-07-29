(* Ocamllex scanner for Crusty *)

{
  open Crustyparse

  let unescape_char c =
    match c with
    | "\\n" -> '\n'
    | "\\t" -> '\t'
    | "\\'" -> '\''
    | "\\\\" -> '\\'
    | _ -> if String.length c = 1 then c.[0] else failwith "Invalid character literal"
    
  let process_escapes s =
    let buffer = Buffer.create (String.length s) in
    let rec aux i =
      if i >= String.length s then Buffer.contents buffer
      else match s.[i] with
        | '\\' when i+1 < String.length s -> (
          match s.[i+1] with
          | 'n' -> Buffer.add_char buffer '\n'; aux (i+2)
          | 't' -> Buffer.add_char buffer '\t'; aux (i+2)
          | 'r' -> Buffer.add_char buffer '\r'; aux (i+2)
          | '"' -> Buffer.add_char buffer '"'; aux (i+2)
          | '\\' -> Buffer.add_char buffer '\\'; aux (i+2)
          | _ -> Buffer.add_char buffer s.[i]; aux (i+1)
        )
        | c -> Buffer.add_char buffer c; aux (i+1)
    in
    aux 0
}

let whitespace = [' ' '\t' '\r' '\n']
let newline = '\r' | '\n' | "\r\n"

let ascii = [^'\'' '\\' '\"'] | '\\' ['\\' '\'' 'n' 't' 'r']
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let int = ['+' '-']? digit+

let float_base = digit* '.' digit+ | digit+ '.' digit*
let float_exp = ['e' 'E'] ['+' '-']? digit+
let float = float_base float_exp? | digit+ float_exp

rule token = parse
  whitespace { token lexbuf }
| "/*"     { comment lexbuf }
| "//"     { line_comment lexbuf }

(* Grouping/Separating *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACK }
| ']'      { RBRACK }
| ';'      { SEMI }
| ','      { COMMA }
| ':'      { EXPLODE }

(* Keywords *)
(* Primitive Types *)
| "int"   { INT }
| "bool"  { BOOL }
| "char"  { CHAR }
| "float" { FLOAT }
| "void"  { VOID }
| "string" { STRING }

(* Type Qualifiers *)
| "ref"   { REF }
| "linear" { LINEAR }
| "unrestricted" { UNRESTRICTED }
| "const" { CONST }

(* Compound Types *)
| "struct" { STRUCT }

(* Operators *)
| '='      { ASSIGN }

(* Arithmetic Operators *)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { STAR }
| '/'      { DIVIDE }
| '%'      { MOD }
| "++"     { INCR }
| "--"     { DECR }

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
| "->"     { ARROW }

(* Control Flow *)
| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
| "break"  { BREAK }
| "continue" { CONTINUE }
| "return" { RETURN }
| "true"   { BOOLLIT(true)  }
| "false"  { BOOLLIT(false) }

(* Identifier *)
| letter (digit | letter | '_')* as lem { ID(lem) }

(* Literals *)
| int as lem  { INTLIT(int_of_string lem) }
| float as lem { FLOATLIT(float_of_string lem) }
| '\'' ascii '\'' as lem { 
    let char_val = process_escapes (String.sub lem 1 (String.length lem - 2)) in
    CHARLIT(char_val.[0])
  }
| '\"' (ascii)* '\"' as lem {
    let string_val = String.sub lem 1 (String.length lem - 2) in
    let processed_string = process_escapes string_val in
    STRINGLIT(processed_string)
  }

(* End of File *)
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and line_comment = parse
  newline { token lexbuf }
| _ { line_comment lexbuf }
