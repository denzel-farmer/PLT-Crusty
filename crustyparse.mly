/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token LPAREN RPAREN LBRACE RBRACE SEMI COLON COMMA
%token INT BOOL CHAR FLOAT VOID
%token INTLIT<int> FLOATLIT<float> BOOLLIT<bool> CHARLIT<char> STRINGLIT<string>
%token REF LINEAR UNRESTRICTED CONST
%token STRUCT

%token ASSIGN
%token PLUST MINUS TIMES DIVIDE MOD INCR DECR
%token EQ NEQ LT LTE GT GTE
%token AND OR NOT
%token DOT 
%token BORROW DEREF ARROW 

%token IF ELSE WHILE BREAK CONTINUE RETURN

%token ID<string>
%token EOF

%start program
%type <Ast.program> program

(* TODO add comma operator here? *)
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LTE GTE
%left PLUS MINUS
%left TIMES DIVIDE MOD

%right INCR DECR NOT
%left DOT BORROW 
(*TODO Prefix and postfix increment and decrement operators *)
(*TODO: unary plus and minus *)
(*TODO: type casting? *)
(*TODO: more complex assignment operators *)

%%
// TODO REMOVE BELOW
/* add function declarations*/
program:
  decls EOF { $1}

decls:
   /* nothing */ { ([], [])               }
 | vdecl SEMI decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl SEMI vdecl_list  {  $1 :: $3 }

/* int x */
vdecl:
  typ ID { ($1, $2) }

typ:
    INT   { Int   }
  | BOOL  { Bool  }

/* fdecl */
fdecl:
  vdecl LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
  {
    {
      rtyp=fst $1;
      fname=snd $1;
      formals=$3;
      locals=$6;
      body=$7
    }
  }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }

stmt:
    expr SEMI                               { Expr $1      }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  /* return */
  | RETURN expr SEMI                        { Return $2      }

expr:
    LITERAL          { Literal($1)            }
  | BLIT             { BoolLit($1)            }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq, $3)     }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | LPAREN expr RPAREN { $2                   }
  /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }






(* Weird constructs 
    - immediate assignments (string literals, structs)
    - struct explosion 
*)