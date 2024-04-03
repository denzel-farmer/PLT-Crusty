/* Ocamlyacc Parser for Crusty */

%{
open Ast
%}

// TODO do we need colon?
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK SEMI COMMA
%token INT BOOL CHAR FLOAT VOID
%token <int> INTLIT
%token <float> FLOATLIT
%token <bool> BOOLLIT
%token <char> CHARLIT
%token <string> STRINGLIT
%token REF LINEAR UNRESTRICTED CONST
%token STRUCT EXPLODE

%token ASSIGN
%token PLUS MINUS TIMES DIVIDE MOD INCR DECR
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
// %left DOT BORROW TODO why doesnt this matter
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
   /* nothing */ { 
    {
        globals = [];
        structs = [];
        funcs = []
    }
   }
| struct_decl SEMI decls {
    {
        globals = $3.globals;
        structs = $1 :: $3.structs;
        funcs = $3.funcs
    }
}
 | const_qualified_var_decl SEMI decls {
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
  | const_qualified_var_decl SEMI var_decl_list  {  $1 :: $3 }
  // | const_array_decl SEMI var_decl_list { $1 :: $3 }

const_qualified_var_decl: 
  var_decl { Var($1) }
  | CONST var_decl { Const($2) }

/* Returns record of linear qualifier, type, and name for variable declaration */
var_decl:
  lin_qual primtyp ID { ($1, $2, $3) }
  | lin_qual STRUCT ID ID { ($1, Struct($3), $4) }
  | primtyp ID { (Unrestricted, $1, $2) }
  | STRUCT ID ID { (Linear, Struct($2), $3) }
  | lin_qual primtyp ID LBRACK INTLIT RBRACK { ($1, Arr($2, $5), $3) }

lin_qual:
      UNRESTRICTED { Unrestricted }
    | LINEAR { Linear }

/* Primative types */
/* TODO integrade struct into primtyp? */
primtyp:
    INT   { Int   }
  | BOOL  { Bool  }
  | CHAR  { Char  }
  | FLOAT { Float }

return_stmt : 
  RETURN expr SEMI { Return($2) }
/*TODO allow declaring structs in function body */
/*TODO allow mixed declaration and assignment */

/* Returns record of function return type, name, arguments, local variables, and body */
/* TODO add returning void */
func_decl:
  | primtyp ID LPAREN args_opt RPAREN LBRACE var_decl_list stmt_list return_stmt RBRACE
  {
    {
      rtyp=$1;
      fname=$2;
      args=$4;
      locals=$7;
      body=$8;
      return=$9;
    }
  }
  | STRUCT ID ID LPAREN args_opt RPAREN LBRACE var_decl_list stmt_list return_stmt RBRACE
  {
    {
      rtyp=Struct($2);
      fname=$3;
      args=$5;
      locals=$8;
      body=$9;
      return=$10
    }
  }

/* Returns list of arguments OR empty list */
args_opt :
  VOID { [] }
  | args { $1 }

/* Returns list of arguments */
args :
  var_decl { [(Val, $1)] }
  | REF var_decl { [(Ref, $2)] }
  | var_decl COMMA args { (Val, $1) :: $3 }
  | REF var_decl COMMA args { (Ref, $2) :: $4 }

// const_array_decl : 
//   | lin_qual primtyp ID LBRACK INTLIT RBRACK { ($1, Arr($5), $3) }

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


/* Comma-separated list of expressions for a struct literal (can't be empty) */
exprs_list:
  | expr { [$1] }
  | expr COMMA exprs_list { $1 :: $3 }

// literal_list:
//   | literal { [$1] }
//   | literal COMMA literal_list { $1 :: $3 }

// literals_list:
//   | literal_expression { [$1] }
//   | literal_expression COMMA literal_expression { $1 :: $3 }

/* Expressions */
expr:
    ID               { Id($1)                 }
  | LPAREN expr RPAREN { $2                   }
  | assignment_expression { Assignment $1 }
  | arithmetic_expression { Operation $1 }
  | comparison_expression { Operation $1 }
  | logical_expression { Operation $1 }
  | literal_expression { Literal $1 }
  | access_expression { Operation $1 }
  | ID LPAREN call_args_opt RPAREN { Call ($1, $3)  }

  // struct_expr:
  //   STRUCT expr { StructLit([$2]) }

/* Call arguments (allows borrowing) */
call_args_opt:
  /*nothing*/ { [] }
  | call_args { $1 }

call_args:
  expr  { [$1] }
  | BORROW expr { [Operation(Borrow($2))] }
  | expr COMMA call_args { $1::$3 }
  | BORROW expr COMMA call_args { Operation(Borrow($2)) :: $4 }

/* Expression Subcomponents */
literal_expression:
    | INTLIT { IntLit($1) }
    | BOOLLIT { BoolLit($1) }
    | FLOATLIT { FloatLit($1) }
    | CHARLIT { CharLit($1) }
    | STRINGLIT { StringLit($1) }
    | LBRACE exprs_list RBRACE { StructLit($2) }
    | LBRACK exprs_list RBRACK { ArrayLit($2) }
    (*TODO: figure out a way to differentiate struct and arraylit, 
    do we need to?, or just have arrays take only literals? *)
    // | LBRACE literal_exprs RBRACE { ArrayLit($2) }

// TODO make a really cool explode snytax (consume?)
assignment_expression:
    | ID ASSIGN expr { Assign($1, $3) }
    | EXPLODE LBRACE id_list RBRACE ASSIGN expr { StructExplode($3, $6) }
    | ID DOT ID ASSIGN expr { StructAssign($1, $3, $5) }
    | ID ARROW ID ASSIGN expr { RefStructAssign($1, $3, $5) }

id_list: /* TODO a struct explosion id list always needs a trailing comma*/
    | ID { [$1] }
    | ID COMMA id_list { $1 :: $3 }

arithmetic_expression:
    | expr PLUS expr { ArithOp($1, Add, $3) }
    | expr MINUS expr { ArithOp($1, Sub, $3) }
    | expr TIMES expr { ArithOp($1, Mul, $3) }
    | expr DIVIDE expr { ArithOp($1, Div, $3) }
    | expr MOD expr { ArithOp($1, Mod, $3) }
    | expr INCR { UnArithOp(PreInc, $1) }
    | expr DECR { UnArithOp(PreDec, $1) }
    | INCR expr { UnArithOp(PostInc, $2) }
    | DECR expr { UnArithOp(PostDec, $2) }

comparison_expression:
    | expr EQ expr { CompOp($1, Eq, $3) }
    | expr NEQ expr { CompOp($1, Neq, $3) }
    | expr LT expr { CompOp($1, Lt, $3) }
    | expr LTE expr { CompOp($1, Leq, $3) }
    | expr GT expr { CompOp($1, Gt, $3) }
    | expr GTE expr { CompOp($1, Geq, $3) }

logical_expression:
    | expr AND expr { LogOp($1, And, $3) }
    | expr OR expr { LogOp($1, Or, $3) }
    | NOT expr { UnLogOp(Not, $2) }

access_expression:
    | ID DOT ID { AccessOp(Id($1), Dot, $3) }
    | ID ARROW ID { AccessOp(Id($1), Arrow, $3) }
    | DEREF ID { Deref(Id($2)) }

