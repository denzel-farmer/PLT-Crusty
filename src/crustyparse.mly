/* Ocamlyacc Parser for Crusty */

%{
open Ast
%}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK SEMI COMMA
%token INT BOOL CHAR FLOAT VOID STRING
%token <int> INTLIT
%token <float> FLOATLIT
%token <bool> BOOLLIT
%token <char> CHARLIT
%token <string> STRINGLIT
%token REF LINEAR UNRESTRICTED CONST
%token STRUCT EXPLODE

/* TODO: Keep an eye on deref versus times and precedence now that they are combined as 'star' */
%token ASSIGN
%token PLUS MINUS DIVIDE MOD INCR DECR STAR  /*TIMES*/
%token EQ NEQ LT LTE GT GTE
%token AND OR NOT
%token DOT
%token BORROW /*DEREF*/ ARROW

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
%left /*TIMES*/ DIVIDE MOD
%left STAR /* TODO is deref left associative? Probably should enforce this 'by hand' for deref/times */

%right INCR DECR NOT

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE
// %left DOT BORROW TODO why doesnt this matter

/*TODO: type casting */
/*TODO: more complex assignment operators */
/*TODO: comma operator */
/*TODO: re-add const */

%%
program:
  decls EOF { $1 }

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

/* Linear Qualifier */
lin_qual:
    UNRESTRICTED { Unrestricted }
  | LINEAR { Linear }

/* Primative types */
primtype:
    INT   { Int   }
  | BOOL  { Bool  }
  | CHAR  { Char  }
  | FLOAT { Float }
  | STRING { String }

/* All types except for arrays: primatives, structs, references */
single_type:
  | lin_qual primtype { Prim($1, $2) }
  | primtype { Prim(Unrestricted, $1) }
  | STRUCT ID { Struct($2) } /* the linear qualifier for a struct is stored in the struct definition */
  | REF single_type { Ref($2) } /* type checking now must guarantee REF used only in call args */

/* Returns record of type (lin qual + base) and name for variable declaration */
var_decl:
  | single_type ID { ($1, $2) }
  | single_type ID LBRACK INTLIT RBRACK { (Arr($1, $4), $2) }

/* Returns semicolon-separated list of variable declarations */
var_decl_list:
  /*nothing*/ { [] }
  | var_decl SEMI var_decl_list  {  $1 :: $3 }

/* Returns record of linearity qualifier, struct name, and member declarations */
struct_decl:
  lin_qual STRUCT ID LBRACE var_decl_list RBRACE {
      {
          lin_qual = $1;
          sname = $3;
          fields = $5
      }
    }
  | STRUCT ID LBRACE var_decl_list RBRACE {
      {
          lin_qual = Linear;
          sname = $2;
          fields = $4
      }
    }

/* Returns record of function return type, name, arguments, local variables, and body */
func_decl:
  single_type ID LPAREN args_opt RPAREN LBRACE var_decl_list stmt_list return_stmt RBRACE
    {
      {
        rtyp=Nonvoid($1);
        fname=$2;
        args=$4;
        locals=$7;
        body=$8;
        return=$9;
      }
    }
  | VOID ID LPAREN args_opt RPAREN LBRACE var_decl_list stmt_list RBRACE
    {
      {
        rtyp=Void;
        fname=$2;
        args=$4;
        locals=$7;
        body=$8;
        return=VoidReturn;
      }
    }

/* Returns list of arguments OR empty list */
args_opt :
  /* nothing */ { [] }
  | args { $1 }

/* Returns list of arguments (comma-separated var_decls) */
args :
  var_decl { [$1] }
  | var_decl COMMA args { $1 :: $3 }

/* Comma-separated list of expressions for a struct literal (can't be empty) */
exprs_list:
  | expr { [$1] }
  | expr COMMA exprs_list { $1 :: $3 }

/* Call arguments or nothing */
call_args_opt:
  /*nothing*/ { [] }
  | call_args { $1 }

/* Comma-separated list of expressions for arguments of a function call (allow borrowing) */
call_args:
  expr_with_borrow  { [$1] }
  | expr_with_borrow COMMA call_args { $1::$3 }

/* Expressions with addition of borrow for call arguments*/
expr_with_borrow:
  | expr { $1 }
  | BORROW ID { Operation(Borrow($2)) }

/* Expressions */
expr:
    ID                    { Id($1) }
  | LPAREN expr RPAREN    { $2 }
  | assignment_expression { Assignment $1 }
  | arithmetic_expression { Operation $1 }
  | comparison_expression { Operation $1 }
  | logical_expression    { Operation $1 }
  | literal_expression    { Literal $1 }
  | access_expression     { Operation $1 }
  | ID LPAREN call_args_opt RPAREN { Call ($1, $3)  }

assignment_expression:
  | ID ASSIGN expr { Assign($1, $3) }
  | EXPLODE LBRACE id_list RBRACE ASSIGN expr { StructExplode($3, $6) } /* Syntax: : {a,b}*/ // TODO make a really cool explode snytax (consume?)
  | ID DOT ID ASSIGN expr { StructAssign($1, $3, $5) }
  | ID ARROW ID ASSIGN expr { RefStructAssign($1, $3, $5) }

/* A list of identifiers to be the LHS of struct explosion */
id_list:
  | ID { [$1] }
  | ID COMMA id_list { $1 :: $3 }

arithmetic_expression:
  | expr PLUS expr { ArithOp($1, Add, $3) }
  | expr MINUS expr { ArithOp($1, Sub, $3) }
  | expr STAR expr { ArithOp($1, Mul, $3) }
  | expr DIVIDE expr { ArithOp($1, Div, $3) }
  | expr MOD expr { ArithOp($1, Mod, $3) }
  | expr INCR { UnArithOp(PreInc, $1) }
  | expr DECR { UnArithOp(PreDec, $1) }
  | INCR expr { UnArithOp(PostInc, $2) }
  | DECR expr { UnArithOp(PostDec, $2) }
  | MINUS expr { UnArithOp(Neg, $2) }

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

/* Literals */
literal_expression:
  | INTLIT { IntLit($1) }
  | BOOLLIT { BoolLit($1) }
  | FLOATLIT { FloatLit($1) }
  | CHARLIT { CharLit($1) }
  | STRINGLIT { StringLit($1) }
  | LBRACE STRUCT ID ARROW exprs_list RBRACE { StructLit($3, $5) }
  | LBRACK exprs_list RBRACK { ArrayLit($2) }

/* Accesses: struct.field, (ref struct)->field, *ref, array[index] */
access_expression:
  | ID DOT ID { AccessOp($1, Dot, $3) }
  | ID ARROW ID { AccessOp($1, Arrow, $3) }
  | STAR ID { Deref($2) } 
  | ID LBRACK expr RBRACK { Index($1, $3) }

/* Returns list of statements OR empty list */
stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }

/* Statements */
stmt:
  | LBRACE var_decl_list stmt_list RBRACE                 { Block ($2, $3) }
  | expr SEMI                               { Expr $1      }
  | ifstmt                                  { $1 }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  | BREAK SEMI                              { Break          }
  | CONTINUE SEMI                           { Continue       }

ifstmt:
  | IF LPAREN expr RPAREN stmt %prec LOWER_THAN_ELSE { If($3, $5, Block([], [])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) } 

return_stmt : 
  RETURN expr SEMI { Return($2) }
  | RETURN SEMI { VoidReturn }

/*TODO allow declaring structs in function body? */
/*TODO allow mixed declaration and assignment? */
