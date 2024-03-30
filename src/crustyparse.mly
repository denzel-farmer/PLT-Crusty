/* Ocamlyacc Parser for Crusty */

%{
open Ast
%}

%token LPAREN RPAREN LBRACE RBRACE SEMI COLON COMMA
%token INT BOOL CHAR FLOAT VOID
%token <int> INTLIT
%token <float> FLOATLIT
%token <bool> BOOLLIT
%token <char> CHARLIT
%token <string> STRINGLIT
%token REF LINEAR UNRESTRICTED CONST
%token STRUCT

%token ASSIGN
%token PLUST MINUS TIMES DIVIDE MOD INCR DECR
%token EQ NEQ LT LTE GT GTE
%token AND OR NOT
%token DOT
%token BORROW DEREF ARROW

%token IF ELSE WHILE BREAK CONTINUE RETURN

%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LTE GTE
%left PLUS MINUS
%left TIMES DIVIDE MOD

%right INCR DECR NOT
%left DOT BORROW
/*TODO Prefix and postfix increment and decrement operators */
/*TODO: unary plus and minus */
/*TODO: type casting */
/*TODO: more complex assignment operators */
/*TODO: comma operator */
/*TODO: arrays */

%%
program:
  decls EOF { $1}

/* Returns record of globals, structs, and functions */
decls:
   /* nothing */ { ([], [])}
| struct_decl SEMI decls {
    {
        globals = $3.globals;
        structs = $1 :: $3.structs;
        funcs = $3.funcs
    }
}
 | var_decl SEMI decls {
    {
        globals = $1 :: $3.globals;
        structs = $3.structs;
        funcs = $3.funcs
    }
 }
 | func_decl decls {
    {
        globals = $2.globals;
        structs = $2.structs;
        funcs = $1 :: $2.funcs
    }
}

/* Returns record of struct name and member declarations */
struct_decl:
  STRUCT ID LBRACE var_decl_list RBRACE {
    {
        sname = $2;
        fields = $4
    }
   }

/* Returns list of variable declarations */
var_decl_list:
  /*nothing*/ { [] }
  | var_decl SEMI vdecl_list  {  $1 :: $3 }

/* Returns record of qualifiers, type, and name for variable declaration */
var_decl:
  const_qual lin_qual primtyp ID { ($1, $2, $3, $4) }
  | const_qual lin_qual STRUCT ID ID { ($1, $2, Struct($4), $5) }
  | const_qual primtyp ID { ($1, Unrestricted, $2, $3) }
  | const_qual STRUCT ID ID { ($1, Linear, Struct($3), $4) }


/* Qualifiers for variable declaration */
const_qual:
/* nothing */ { Var }
    | CONST { Const }

lin_qual:
      UNRESTRICTED { Unrestricted }
    | LINEAR { Linear }

/* Primative types */
primtyp:
    INT   { Int   }
  | BOOL  { Bool  }
  | CHAR  { Char  }
  | FLOAT { Float }

/* Function return type (includes void) */
ret_typ :
    | primtyp { $1}
    | STRUCT ID { $2}
    | VOID { Void }

/*TODO allow declaring structs in function body */
/*TODO allow mixed declaration and assignment */

/* Returns record of function return type, name, arguments, local variables, and body */
fdecl:
  ret_typ ID LPAREN args_opt RPAREN LBRACE var_decl_list stmt_list RBRACE
  {
    {
      rtyp=$1;
      fname=$2;
      args=$4;
      locals=$7;
      body=$8
    }
  }

/* Returns list of arguments OR empty list */
args_opt :
  VOID { [] }
  | args { $1 }

/* Returns list of arguments */
args :
  vdecl { [(Val, $1)] }
  | REF vdecl { [(Ref, $2)] }
  | vdecl COMMA args { (Val, $1) :: $3 }
  | REF vdecl COMMA args { (Ref, $2) :: $4 }


/* Returns list of statements OR empty list */
stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }

/* Statements */
stmt:
    expr SEMI                               { Expr $1      }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  | BREAK SEMI                              { Break          }
  | CONTINUE SEMI                           { Continue       }
  | RETURN expr SEMI                        { Return $2      }

/* Expressions */
expr:
    ID               { Id($1)                 }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | LPAREN expr RPAREN { $2                   }
  | assignment_expression { Assignment $1 }
  | arithmetic_expression { Operation $1 }
  | comparison_expression { Operation $1 }
  | logical_expression { Operation $1 }
  | access_expression { AccessOp $1 }
  | ID LPAREN call_args_opt RPAREN { Call ($1, $3)  }

/* Call arguments (allows borrowing) */
call_args_opt:
  /*nothing*/ { [] }
  | call_args { $1 }

call_args:
  expr  { [$1] }
  | BORROW expr { [Borrow($2)] }
  | expr COMMA call_args { $1::$3 }
  | BORROW expr COMMA call_args { Borrow($2) :: $4 }

/* Expression Subcomponents */
literal_expression:
    | INTLIT { IntLit($1) }
    | BOOLLIT { BoolLit($1) }
    | FLOATLIT { FloatLit($1) }
    | CHARLIT { CharLit($1) }
    | STRINGLIT { StringLit($1) }
    | LBRACE expr_list RBRACE { StructLit($2) }

assignment_expression:
    | ID ASSIGN expr { Assign($1, $3) }
    | LBRACE id_list RBRACE ASSIGN expr { StructExplode($2, $5) }
    | ID ASSIGN ID PERIOD expr { StructAssign($1, $3, $5) }
    | ID ASSIGN ID ARROW expr { RefStructAssign($1, $3, $5) }

id_list:
    | ID { [$1] }
    | ID COMMA id_list { $1 :: $3 }

arithmetic_expression:
    | expr PLUS expr { ArithOp($1, Add, $3) }
    | expr MINUS expr { ArithOp($1, Sub, $3) }
    | expr TIMES expr { ArithOp($1, Mul, $3) }
    | expr DIVIDE expr { ArithOp($1, Div, $3) }
    | expr MOD expr { ArithOp($1, Mod, $3) }
    | expr INCR { UnArithOp(PreIncr, $1) }
    | expr DECR { UnArithOp(PreDecr, $1) }
    | INCR expr { UnArithOp(PostIncr, $2) }
    | DECR expr { UnArithOp(PostDecr, $2) }

comparison_expression:
    | expr EQ expr { CompOp($1, Eq, $3) }
    | expr NEQ expr { CompOp($1, Neq, $3) }
    | expr LT expr { CompOp($1, Lt, $3) }
    | expr LTE expr { CompOp($1, Lte, $3) }
    | expr GT expr { CompOp($1, Gt, $3) }
    | expr GTE expr { CompOp($1, Gte, $3) }

logical_expression:
    | expr AND expr { LogOp($1, And, $3) }
    | expr OR expr { LogOp($1, Or, $3) }
    | NOT expr { UnLogOp(Not, $2) }

access_expression:
    | ID PERIOD ID { AccessOp($1, Dot, $3) }
    | ID ARROW ID { AccessOp($1, Arrow, $3) }
    | DEREF ID { UnOp(Deref, $2) }

