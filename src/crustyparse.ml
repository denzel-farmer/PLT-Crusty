
type token = 
  | WHILE
  | VOID
  | UNRESTRICTED
  | STRUCT
  | STRINGLIT of (
# 13 "crustyparse.mly"
       (string)
# 11 "crustyparse.ml"
)
  | STRING
  | STAR
  | SEMI
  | RPAREN
  | RETURN
  | REF
  | RBRACK
  | RBRACE
  | PLUS
  | OR
  | NOT
  | NEQ
  | MOD
  | MINUS
  | LTE
  | LT
  | LPAREN
  | LINEAR
  | LBRACK
  | LBRACE
  | INTLIT of (
# 9 "crustyparse.mly"
       (int)
# 36 "crustyparse.ml"
)
  | INT
  | INCR
  | IF
  | ID of (
# 27 "crustyparse.mly"
       (string)
# 44 "crustyparse.ml"
)
  | GTE
  | GT
  | FLOATLIT of (
# 10 "crustyparse.mly"
       (float)
# 51 "crustyparse.ml"
)
  | FLOAT
  | EXPLODE
  | EQ
  | EOF
  | ELSE
  | DOT
  | DIVIDE
  | DECR
  | CONTINUE
  | CONST
  | COMMA
  | CHARLIT of (
# 12 "crustyparse.mly"
       (char)
# 67 "crustyparse.ml"
)
  | CHAR
  | BREAK
  | BORROW
  | BOOLLIT of (
# 11 "crustyparse.mly"
       (bool)
# 75 "crustyparse.ml"
)
  | BOOL
  | ASSIGN
  | ARROW
  | AND

# 3 "crustyparse.mly"
  
open Ast

# 86 "crustyparse.ml"

let menhir_begin_marker =
  0

and (xv_var_decl_list, xv_var_decl, xv_struct_decl, xv_stmt_list, xv_stmt, xv_single_type, xv_return_stmt, xv_program, xv_primtype, xv_logical_expression, xv_literal_expression, xv_lin_qual, xv_ifstmt, xv_id_list, xv_func_decl, xv_exprs_list, xv_expr_with_borrow, xv_expr, xv_decls, xv_comparison_expression, xv_call_args_opt, xv_call_args, xv_assignment_expression, xv_arithmetic_expression, xv_args_opt, xv_args, xv_access_expression) =
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 116 "crustyparse.mly"
                 _3
# 95 "crustyparse.ml"
   : 'tv_var_decl_list) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 116 "crustyparse.mly"
            _2
# 99 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 116 "crustyparse.mly"
   _1
# 103 "crustyparse.ml"
   : 'tv_var_decl) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_var_decl_list ->
    
# 116 "crustyparse.mly"
                                 (  _1 :: _3 )
# 108 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) : 'tv_var_decl_list ->
    
# 115 "crustyparse.mly"
              ( [] )
# 114 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 111 "crustyparse.mly"
                                _5
# 119 "crustyparse.ml"
   : unit) (_startpos__5_ : Lexing.position) (_endpos__5_ : Lexing.position) (_startofs__5_ : int) (_endofs__5_ : int) (_loc__5_ : Lexing.position * Lexing.position) (
# 111 "crustyparse.mly"
                         _4
# 123 "crustyparse.ml"
   : (
# 9 "crustyparse.mly"
       (int)
# 127 "crustyparse.ml"
  )) (_startpos__4_ : Lexing.position) (_endpos__4_ : Lexing.position) (_startofs__4_ : int) (_endofs__4_ : int) (_loc__4_ : Lexing.position * Lexing.position) (
# 111 "crustyparse.mly"
                  _3
# 131 "crustyparse.ml"
   : unit) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 111 "crustyparse.mly"
               _2
# 135 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 139 "crustyparse.ml"
  )) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 111 "crustyparse.mly"
   _1
# 143 "crustyparse.ml"
   : 'tv_single_type) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_var_decl ->
    
# 111 "crustyparse.mly"
                                        ( (Arr(_1, _4), _2) )
# 148 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 110 "crustyparse.mly"
               _2
# 153 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 157 "crustyparse.ml"
  )) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 110 "crustyparse.mly"
   _1
# 161 "crustyparse.ml"
   : 'tv_single_type) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_var_decl ->
    
# 110 "crustyparse.mly"
                   ( (_1, _2) )
# 166 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 127 "crustyparse.mly"
                                  _5
# 171 "crustyparse.ml"
   : unit) (_startpos__5_ : Lexing.position) (_endpos__5_ : Lexing.position) (_startofs__5_ : int) (_endofs__5_ : int) (_loc__5_ : Lexing.position * Lexing.position) (
# 127 "crustyparse.mly"
                    _4
# 175 "crustyparse.ml"
   : 'tv_var_decl_list) (_startpos__4_ : Lexing.position) (_endpos__4_ : Lexing.position) (_startofs__4_ : int) (_endofs__4_ : int) (_loc__4_ : Lexing.position * Lexing.position) (
# 127 "crustyparse.mly"
             _3
# 179 "crustyparse.ml"
   : unit) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 127 "crustyparse.mly"
          _2
# 183 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 187 "crustyparse.ml"
  )) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 127 "crustyparse.mly"
   _1
# 191 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_struct_decl ->
    
# 127 "crustyparse.mly"
                                          (
      {
          lin_qual = Linear;
          sname = _2;
          fields = _4
      }
    )
# 202 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 120 "crustyparse.mly"
                                         _6
# 207 "crustyparse.ml"
   : unit) (_startpos__6_ : Lexing.position) (_endpos__6_ : Lexing.position) (_startofs__6_ : int) (_endofs__6_ : int) (_loc__6_ : Lexing.position * Lexing.position) (
# 120 "crustyparse.mly"
                           _5
# 211 "crustyparse.ml"
   : 'tv_var_decl_list) (_startpos__5_ : Lexing.position) (_endpos__5_ : Lexing.position) (_startofs__5_ : int) (_endofs__5_ : int) (_loc__5_ : Lexing.position * Lexing.position) (
# 120 "crustyparse.mly"
                    _4
# 215 "crustyparse.ml"
   : unit) (_startpos__4_ : Lexing.position) (_endpos__4_ : Lexing.position) (_startofs__4_ : int) (_endofs__4_ : int) (_loc__4_ : Lexing.position * Lexing.position) (
# 120 "crustyparse.mly"
                 _3
# 219 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 223 "crustyparse.ml"
  )) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 120 "crustyparse.mly"
          _2
# 227 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 119 "crustyparse.mly"
            _1
# 232 "crustyparse.ml"
   : 'tv_lin_qual) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_struct_decl ->
    
# 120 "crustyparse.mly"
                                                 (
      {
          lin_qual = _1;
          sname = _3;
          fields = _5
      }
    )
# 243 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 258 "crustyparse.mly"
        _2
# 248 "crustyparse.ml"
   : 'tv_stmt_list) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 258 "crustyparse.mly"
   _1
# 252 "crustyparse.ml"
   : 'tv_stmt) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_stmt_list ->
    
# 258 "crustyparse.mly"
                    ( _1::_2 )
# 257 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) : 'tv_stmt_list ->
    
# 257 "crustyparse.mly"
                ( [] )
# 263 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 267 "crustyparse.mly"
            _2
# 268 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 267 "crustyparse.mly"
   _1
# 272 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_stmt ->
    
# 267 "crustyparse.mly"
                                            ( Continue       )
# 277 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 266 "crustyparse.mly"
         _2
# 282 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 266 "crustyparse.mly"
   _1
# 286 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_stmt ->
    
# 266 "crustyparse.mly"
                                            ( Break          )
# 291 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 265 "crustyparse.mly"
                            _5
# 296 "crustyparse.ml"
   : 'tv_stmt) (_startpos__5_ : Lexing.position) (_endpos__5_ : Lexing.position) (_startofs__5_ : int) (_endofs__5_ : int) (_loc__5_ : Lexing.position * Lexing.position) (
# 265 "crustyparse.mly"
                     _4
# 300 "crustyparse.ml"
   : unit) (_startpos__4_ : Lexing.position) (_endpos__4_ : Lexing.position) (_startofs__4_ : int) (_endofs__4_ : int) (_loc__4_ : Lexing.position * Lexing.position) (
# 265 "crustyparse.mly"
                _3
# 304 "crustyparse.ml"
   : 'tv_expr) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 265 "crustyparse.mly"
         _2
# 308 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 265 "crustyparse.mly"
   _1
# 312 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_stmt ->
    
# 265 "crustyparse.mly"
                                            ( While (_3, _5)  )
# 317 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 264 "crustyparse.mly"
   _1
# 322 "crustyparse.ml"
   : 'tv_ifstmt) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_stmt ->
    
# 264 "crustyparse.mly"
                                            ( _1 )
# 327 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 263 "crustyparse.mly"
        _2
# 332 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 263 "crustyparse.mly"
   _1
# 336 "crustyparse.ml"
   : 'tv_expr) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_stmt ->
    
# 263 "crustyparse.mly"
                                            ( Expr _1      )
# 341 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 262 "crustyparse.mly"
                                  _4
# 346 "crustyparse.ml"
   : unit) (_startpos__4_ : Lexing.position) (_endpos__4_ : Lexing.position) (_startofs__4_ : int) (_endofs__4_ : int) (_loc__4_ : Lexing.position * Lexing.position) (
# 262 "crustyparse.mly"
                        _3
# 350 "crustyparse.ml"
   : 'tv_stmt_list) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 262 "crustyparse.mly"
          _2
# 354 "crustyparse.ml"
   : 'tv_var_decl_list) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 262 "crustyparse.mly"
   _1
# 358 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_stmt ->
    
# 262 "crustyparse.mly"
                                                          ( Block (_2, _3) )
# 363 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 106 "crustyparse.mly"
       _2
# 368 "crustyparse.ml"
   : 'tv_single_type) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 106 "crustyparse.mly"
   _1
# 372 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_single_type ->
    
# 106 "crustyparse.mly"
                    ( Ref(_2) )
# 377 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 105 "crustyparse.mly"
          _2
# 382 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 386 "crustyparse.ml"
  )) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 105 "crustyparse.mly"
   _1
# 390 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_single_type ->
    
# 105 "crustyparse.mly"
              ( Struct(_2) )
# 395 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 104 "crustyparse.mly"
   _1
# 400 "crustyparse.ml"
   : 'tv_primtype) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_single_type ->
    
# 104 "crustyparse.mly"
             ( Prim(Unrestricted, _1) )
# 405 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 103 "crustyparse.mly"
            _2
# 410 "crustyparse.ml"
   : 'tv_primtype) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 103 "crustyparse.mly"
   _1
# 414 "crustyparse.ml"
   : 'tv_lin_qual) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_single_type ->
    
# 103 "crustyparse.mly"
                      ( Prim(_1, _2) )
# 419 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 275 "crustyparse.mly"
          _2
# 424 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 275 "crustyparse.mly"
   _1
# 428 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_return_stmt ->
    
# 275 "crustyparse.mly"
                ( VoidReturn )
# 433 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 274 "crustyparse.mly"
             _3
# 438 "crustyparse.ml"
   : unit) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 274 "crustyparse.mly"
        _2
# 442 "crustyparse.ml"
   : 'tv_expr) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 273 "crustyparse.mly"
             _1
# 447 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_return_stmt ->
    
# 274 "crustyparse.mly"
                   ( Return(_2) )
# 452 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 55 "crustyparse.mly"
       _2
# 457 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 54 "crustyparse.mly"
        _1
# 462 "crustyparse.ml"
   : 'tv_decls) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : (
# 31 "crustyparse.mly"
      (Ast.program)
# 466 "crustyparse.ml"
  ) ->
    (
# 55 "crustyparse.mly"
            ( _1 )
# 471 "crustyparse.ml"
     : 'tv_program) in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 99 "crustyparse.mly"
   _1
# 476 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_primtype ->
    
# 99 "crustyparse.mly"
           ( String )
# 481 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 98 "crustyparse.mly"
   _1
# 486 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_primtype ->
    
# 98 "crustyparse.mly"
          ( Float )
# 491 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 97 "crustyparse.mly"
   _1
# 496 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_primtype ->
    
# 97 "crustyparse.mly"
          ( Char  )
# 501 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 96 "crustyparse.mly"
   _1
# 506 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_primtype ->
    
# 96 "crustyparse.mly"
          ( Bool  )
# 511 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 94 "crustyparse.mly"
         _1
# 517 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_primtype ->
    
# 95 "crustyparse.mly"
          ( Int   )
# 522 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 236 "crustyparse.mly"
       _2
# 527 "crustyparse.ml"
   : 'tv_expr) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 236 "crustyparse.mly"
   _1
# 531 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_logical_expression ->
    
# 236 "crustyparse.mly"
             ( UnLogOp(Not, _2) )
# 536 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 235 "crustyparse.mly"
           _3
# 541 "crustyparse.ml"
   : 'tv_expr) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 235 "crustyparse.mly"
        _2
# 545 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 235 "crustyparse.mly"
   _1
# 549 "crustyparse.ml"
   : 'tv_expr) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_logical_expression ->
    
# 235 "crustyparse.mly"
                 ( LogOp(_1, Or, _3) )
# 554 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 234 "crustyparse.mly"
            _3
# 559 "crustyparse.ml"
   : 'tv_expr) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 234 "crustyparse.mly"
        _2
# 563 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 234 "crustyparse.mly"
   _1
# 567 "crustyparse.ml"
   : 'tv_expr) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_logical_expression ->
    
# 234 "crustyparse.mly"
                  ( LogOp(_1, And, _3) )
# 572 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 246 "crustyparse.mly"
                     _3
# 577 "crustyparse.ml"
   : unit) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 246 "crustyparse.mly"
          _2
# 581 "crustyparse.ml"
   : 'tv_exprs_list) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 246 "crustyparse.mly"
   _1
# 585 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_literal_expression ->
    
# 246 "crustyparse.mly"
                             ( ArrayLit(_2) )
# 590 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 245 "crustyparse.mly"
                                     _6
# 595 "crustyparse.ml"
   : unit) (_startpos__6_ : Lexing.position) (_endpos__6_ : Lexing.position) (_startofs__6_ : int) (_endofs__6_ : int) (_loc__6_ : Lexing.position * Lexing.position) (
# 245 "crustyparse.mly"
                          _5
# 599 "crustyparse.ml"
   : 'tv_exprs_list) (_startpos__5_ : Lexing.position) (_endpos__5_ : Lexing.position) (_startofs__5_ : int) (_endofs__5_ : int) (_loc__5_ : Lexing.position * Lexing.position) (
# 245 "crustyparse.mly"
                    _4
# 603 "crustyparse.ml"
   : unit) (_startpos__4_ : Lexing.position) (_endpos__4_ : Lexing.position) (_startofs__4_ : int) (_endofs__4_ : int) (_loc__4_ : Lexing.position * Lexing.position) (
# 245 "crustyparse.mly"
                 _3
# 607 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 611 "crustyparse.ml"
  )) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 245 "crustyparse.mly"
          _2
# 615 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 245 "crustyparse.mly"
   _1
# 619 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_literal_expression ->
    
# 245 "crustyparse.mly"
                                             ( StructLit(_3, _5) )
# 624 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 244 "crustyparse.mly"
   _1
# 629 "crustyparse.ml"
   : (
# 13 "crustyparse.mly"
       (string)
# 633 "crustyparse.ml"
  )) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_literal_expression ->
    
# 244 "crustyparse.mly"
              ( StringLit(_1) )
# 638 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 243 "crustyparse.mly"
   _1
# 643 "crustyparse.ml"
   : (
# 12 "crustyparse.mly"
       (char)
# 647 "crustyparse.ml"
  )) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_literal_expression ->
    
# 243 "crustyparse.mly"
            ( CharLit(_1) )
# 652 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 242 "crustyparse.mly"
   _1
# 657 "crustyparse.ml"
   : (
# 10 "crustyparse.mly"
       (float)
# 661 "crustyparse.ml"
  )) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_literal_expression ->
    
# 242 "crustyparse.mly"
             ( FloatLit(_1) )
# 666 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 241 "crustyparse.mly"
   _1
# 671 "crustyparse.ml"
   : (
# 11 "crustyparse.mly"
       (bool)
# 675 "crustyparse.ml"
  )) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_literal_expression ->
    
# 241 "crustyparse.mly"
            ( BoolLit(_1) )
# 680 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 240 "crustyparse.mly"
   _1
# 685 "crustyparse.ml"
   : (
# 9 "crustyparse.mly"
       (int)
# 689 "crustyparse.ml"
  )) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_literal_expression ->
    
# 240 "crustyparse.mly"
           ( IntLit(_1) )
# 694 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 91 "crustyparse.mly"
   _1
# 699 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_lin_qual ->
    
# 91 "crustyparse.mly"
           ( Linear )
# 704 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 89 "crustyparse.mly"
         _1
# 710 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_lin_qual ->
    
# 90 "crustyparse.mly"
                 ( Unrestricted )
# 715 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 271 "crustyparse.mly"
                                   _7
# 720 "crustyparse.ml"
   : 'tv_stmt) (_startpos__7_ : Lexing.position) (_endpos__7_ : Lexing.position) (_startofs__7_ : int) (_endofs__7_ : int) (_loc__7_ : Lexing.position * Lexing.position) (
# 271 "crustyparse.mly"
                              _6
# 724 "crustyparse.ml"
   : unit) (_startpos__6_ : Lexing.position) (_endpos__6_ : Lexing.position) (_startofs__6_ : int) (_endofs__6_ : int) (_loc__6_ : Lexing.position * Lexing.position) (
# 271 "crustyparse.mly"
                         _5
# 728 "crustyparse.ml"
   : 'tv_stmt) (_startpos__5_ : Lexing.position) (_endpos__5_ : Lexing.position) (_startofs__5_ : int) (_endofs__5_ : int) (_loc__5_ : Lexing.position * Lexing.position) (
# 271 "crustyparse.mly"
                  _4
# 732 "crustyparse.ml"
   : unit) (_startpos__4_ : Lexing.position) (_endpos__4_ : Lexing.position) (_startofs__4_ : int) (_endofs__4_ : int) (_loc__4_ : Lexing.position * Lexing.position) (
# 271 "crustyparse.mly"
             _3
# 736 "crustyparse.ml"
   : 'tv_expr) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 271 "crustyparse.mly"
      _2
# 740 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 271 "crustyparse.mly"
   _1
# 744 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_ifstmt ->
    
# 271 "crustyparse.mly"
                                         ( If(_3, _5, _7) )
# 749 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 270 "crustyparse.mly"
                         _5
# 754 "crustyparse.ml"
   : 'tv_stmt) (_startpos__5_ : Lexing.position) (_endpos__5_ : Lexing.position) (_startofs__5_ : int) (_endofs__5_ : int) (_loc__5_ : Lexing.position * Lexing.position) (
# 270 "crustyparse.mly"
                  _4
# 758 "crustyparse.ml"
   : unit) (_startpos__4_ : Lexing.position) (_endpos__4_ : Lexing.position) (_startofs__4_ : int) (_endofs__4_ : int) (_loc__4_ : Lexing.position * Lexing.position) (
# 270 "crustyparse.mly"
             _3
# 762 "crustyparse.ml"
   : 'tv_expr) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 270 "crustyparse.mly"
      _2
# 766 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 270 "crustyparse.mly"
   _1
# 770 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_ifstmt ->
    
# 270 "crustyparse.mly"
                                                     ( If(_3, _5, Block([], [])) )
# 775 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 211 "crustyparse.mly"
            _3
# 780 "crustyparse.ml"
   : 'tv_id_list) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 211 "crustyparse.mly"
      _2
# 784 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 211 "crustyparse.mly"
   _1
# 788 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 792 "crustyparse.ml"
  )) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_id_list ->
    
# 211 "crustyparse.mly"
                     ( _1 :: _3 )
# 797 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 210 "crustyparse.mly"
   _1
# 802 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 806 "crustyparse.ml"
  )) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_id_list ->
    
# 210 "crustyparse.mly"
       ( [_1] )
# 811 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 148 "crustyparse.mly"
                                                                 _9
# 816 "crustyparse.ml"
   : unit) (_startpos__9_ : Lexing.position) (_endpos__9_ : Lexing.position) (_startofs__9_ : int) (_endofs__9_ : int) (_loc__9_ : Lexing.position * Lexing.position) (
# 148 "crustyparse.mly"
                                                       _8
# 820 "crustyparse.ml"
   : 'tv_stmt_list) (_startpos__8_ : Lexing.position) (_endpos__8_ : Lexing.position) (_startofs__8_ : int) (_endofs__8_ : int) (_loc__8_ : Lexing.position * Lexing.position) (
# 148 "crustyparse.mly"
                                         _7
# 824 "crustyparse.ml"
   : 'tv_var_decl_list) (_startpos__7_ : Lexing.position) (_endpos__7_ : Lexing.position) (_startofs__7_ : int) (_endofs__7_ : int) (_loc__7_ : Lexing.position * Lexing.position) (
# 148 "crustyparse.mly"
                                  _6
# 828 "crustyparse.ml"
   : unit) (_startpos__6_ : Lexing.position) (_endpos__6_ : Lexing.position) (_startofs__6_ : int) (_endofs__6_ : int) (_loc__6_ : Lexing.position * Lexing.position) (
# 148 "crustyparse.mly"
                           _5
# 832 "crustyparse.ml"
   : unit) (_startpos__5_ : Lexing.position) (_endpos__5_ : Lexing.position) (_startofs__5_ : int) (_endofs__5_ : int) (_loc__5_ : Lexing.position * Lexing.position) (
# 148 "crustyparse.mly"
                  _4
# 836 "crustyparse.ml"
   : 'tv_args_opt) (_startpos__4_ : Lexing.position) (_endpos__4_ : Lexing.position) (_startofs__4_ : int) (_endofs__4_ : int) (_loc__4_ : Lexing.position * Lexing.position) (
# 148 "crustyparse.mly"
           _3
# 840 "crustyparse.ml"
   : unit) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 148 "crustyparse.mly"
        _2
# 844 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 848 "crustyparse.ml"
  )) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 148 "crustyparse.mly"
   _1
# 852 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_func_decl ->
    
# 149 "crustyparse.mly"
    (
      {
        rtyp=Void;
        fname=_2;
        args=_4;
        locals=_7;
        body=_8;
        return=VoidReturn;
      }
    )
# 866 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 137 "crustyparse.mly"
                                                                                  _10
# 871 "crustyparse.ml"
   : unit) (_startpos__10_ : Lexing.position) (_endpos__10_ : Lexing.position) (_startofs__10_ : int) (_endofs__10_ : int) (_loc__10_ : Lexing.position * Lexing.position) (
# 137 "crustyparse.mly"
                                                                      _9
# 875 "crustyparse.ml"
   : 'tv_return_stmt) (_startpos__9_ : Lexing.position) (_endpos__9_ : Lexing.position) (_startofs__9_ : int) (_endofs__9_ : int) (_loc__9_ : Lexing.position * Lexing.position) (
# 137 "crustyparse.mly"
                                                            _8
# 879 "crustyparse.ml"
   : 'tv_stmt_list) (_startpos__8_ : Lexing.position) (_endpos__8_ : Lexing.position) (_startofs__8_ : int) (_endofs__8_ : int) (_loc__8_ : Lexing.position * Lexing.position) (
# 137 "crustyparse.mly"
                                              _7
# 883 "crustyparse.ml"
   : 'tv_var_decl_list) (_startpos__7_ : Lexing.position) (_endpos__7_ : Lexing.position) (_startofs__7_ : int) (_endofs__7_ : int) (_loc__7_ : Lexing.position * Lexing.position) (
# 137 "crustyparse.mly"
                                       _6
# 887 "crustyparse.ml"
   : unit) (_startpos__6_ : Lexing.position) (_endpos__6_ : Lexing.position) (_startofs__6_ : int) (_endofs__6_ : int) (_loc__6_ : Lexing.position * Lexing.position) (
# 137 "crustyparse.mly"
                                _5
# 891 "crustyparse.ml"
   : unit) (_startpos__5_ : Lexing.position) (_endpos__5_ : Lexing.position) (_startofs__5_ : int) (_endofs__5_ : int) (_loc__5_ : Lexing.position * Lexing.position) (
# 137 "crustyparse.mly"
                       _4
# 895 "crustyparse.ml"
   : 'tv_args_opt) (_startpos__4_ : Lexing.position) (_endpos__4_ : Lexing.position) (_startofs__4_ : int) (_endofs__4_ : int) (_loc__4_ : Lexing.position * Lexing.position) (
# 137 "crustyparse.mly"
                _3
# 899 "crustyparse.ml"
   : unit) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 137 "crustyparse.mly"
             _2
# 903 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 907 "crustyparse.ml"
  )) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 136 "crustyparse.mly"
          _1
# 912 "crustyparse.ml"
   : 'tv_single_type) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_func_decl ->
    
# 138 "crustyparse.mly"
    (
      {
        rtyp=Nonvoid(_1);
        fname=_2;
        args=_4;
        locals=_7;
        body=_8;
        return=_9;
      }
    )
# 926 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 173 "crustyparse.mly"
              _3
# 931 "crustyparse.ml"
   : 'tv_exprs_list) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 173 "crustyparse.mly"
        _2
# 935 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 173 "crustyparse.mly"
   _1
# 939 "crustyparse.ml"
   : 'tv_expr) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_exprs_list ->
    
# 173 "crustyparse.mly"
                          ( _1 :: _3 )
# 944 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 172 "crustyparse.mly"
   _1
# 949 "crustyparse.ml"
   : 'tv_expr) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_exprs_list ->
    
# 172 "crustyparse.mly"
         ( [_1] )
# 954 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 188 "crustyparse.mly"
          _2
# 959 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 963 "crustyparse.ml"
  )) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 188 "crustyparse.mly"
   _1
# 967 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_expr_with_borrow ->
    
# 188 "crustyparse.mly"
              ( Operation(Borrow(_2)) )
# 972 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 187 "crustyparse.mly"
   _1
# 977 "crustyparse.ml"
   : 'tv_expr) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_expr_with_borrow ->
    
# 187 "crustyparse.mly"
         ( _1 )
# 982 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 200 "crustyparse.mly"
                           _4
# 987 "crustyparse.ml"
   : unit) (_startpos__4_ : Lexing.position) (_endpos__4_ : Lexing.position) (_startofs__4_ : int) (_endofs__4_ : int) (_loc__4_ : Lexing.position * Lexing.position) (
# 200 "crustyparse.mly"
             _3
# 991 "crustyparse.ml"
   : 'tv_call_args_opt) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 200 "crustyparse.mly"
      _2
# 995 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 200 "crustyparse.mly"
   _1
# 999 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 1003 "crustyparse.ml"
  )) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_expr ->
    
# 200 "crustyparse.mly"
                                   ( Call (_1, _3)  )
# 1008 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 199 "crustyparse.mly"
   _1
# 1013 "crustyparse.ml"
   : 'tv_access_expression) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_expr ->
    
# 199 "crustyparse.mly"
                          ( Operation _1 )
# 1018 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 198 "crustyparse.mly"
   _1
# 1023 "crustyparse.ml"
   : 'tv_literal_expression) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_expr ->
    
# 198 "crustyparse.mly"
                          ( Literal _1 )
# 1028 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 197 "crustyparse.mly"
   _1
# 1033 "crustyparse.ml"
   : 'tv_logical_expression) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_expr ->
    
# 197 "crustyparse.mly"
                          ( Operation _1 )
# 1038 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 196 "crustyparse.mly"
   _1
# 1043 "crustyparse.ml"
   : 'tv_comparison_expression) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_expr ->
    
# 196 "crustyparse.mly"
                          ( Operation _1 )
# 1048 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 195 "crustyparse.mly"
   _1
# 1053 "crustyparse.ml"
   : 'tv_arithmetic_expression) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_expr ->
    
# 195 "crustyparse.mly"
                          ( Operation _1 )
# 1058 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 194 "crustyparse.mly"
   _1
# 1063 "crustyparse.ml"
   : 'tv_assignment_expression) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_expr ->
    
# 194 "crustyparse.mly"
                          ( Assignment _1 )
# 1068 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 193 "crustyparse.mly"
               _3
# 1073 "crustyparse.ml"
   : unit) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 193 "crustyparse.mly"
          _2
# 1077 "crustyparse.ml"
   : 'tv_expr) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 193 "crustyparse.mly"
   _1
# 1081 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_expr ->
    
# 193 "crustyparse.mly"
                          ( _2 )
# 1086 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 191 "crustyparse.mly"
     _1
# 1092 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 1096 "crustyparse.ml"
  )) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_expr ->
    
# 192 "crustyparse.mly"
                          ( Id(_1) )
# 1101 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 80 "crustyparse.mly"
             _2
# 1106 "crustyparse.ml"
   : 'tv_decls) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 80 "crustyparse.mly"
   _1
# 1110 "crustyparse.ml"
   : 'tv_func_decl) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_decls ->
    
# 80 "crustyparse.mly"
                    (
      {
          globals = _2.globals;
          structs = _2.structs;
          funcs = _1 :: _2.funcs
      }
    )
# 1121 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 73 "crustyparse.mly"
                 _3
# 1126 "crustyparse.ml"
   : 'tv_decls) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 73 "crustyparse.mly"
            _2
# 1130 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 73 "crustyparse.mly"
   _1
# 1134 "crustyparse.ml"
   : 'tv_var_decl) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_decls ->
    
# 73 "crustyparse.mly"
                        (
      {
          globals = _1 :: _3.globals;
          structs = _3.structs;
          funcs = _3.funcs
      }
    )
# 1145 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 66 "crustyparse.mly"
                    _3
# 1150 "crustyparse.ml"
   : 'tv_decls) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 66 "crustyparse.mly"
               _2
# 1154 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 66 "crustyparse.mly"
   _1
# 1158 "crustyparse.ml"
   : 'tv_struct_decl) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_decls ->
    
# 66 "crustyparse.mly"
                           (
      {
          globals = _3.globals;
          structs = _1 :: _3.structs;
          funcs = _3.funcs
      }
    )
# 1169 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) : 'tv_decls ->
    
# 59 "crustyparse.mly"
                ( 
    {
        globals = [];
        structs = [];
        funcs = []
    }
  )
# 1181 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 231 "crustyparse.mly"
            _3
# 1186 "crustyparse.ml"
   : 'tv_expr) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 231 "crustyparse.mly"
        _2
# 1190 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 231 "crustyparse.mly"
   _1
# 1194 "crustyparse.ml"
   : 'tv_expr) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_comparison_expression ->
    
# 231 "crustyparse.mly"
                  ( CompOp(_1, Geq, _3) )
# 1199 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 230 "crustyparse.mly"
           _3
# 1204 "crustyparse.ml"
   : 'tv_expr) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 230 "crustyparse.mly"
        _2
# 1208 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 230 "crustyparse.mly"
   _1
# 1212 "crustyparse.ml"
   : 'tv_expr) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_comparison_expression ->
    
# 230 "crustyparse.mly"
                 ( CompOp(_1, Gt, _3) )
# 1217 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 229 "crustyparse.mly"
            _3
# 1222 "crustyparse.ml"
   : 'tv_expr) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 229 "crustyparse.mly"
        _2
# 1226 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 229 "crustyparse.mly"
   _1
# 1230 "crustyparse.ml"
   : 'tv_expr) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_comparison_expression ->
    
# 229 "crustyparse.mly"
                  ( CompOp(_1, Leq, _3) )
# 1235 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 228 "crustyparse.mly"
           _3
# 1240 "crustyparse.ml"
   : 'tv_expr) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 228 "crustyparse.mly"
        _2
# 1244 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 228 "crustyparse.mly"
   _1
# 1248 "crustyparse.ml"
   : 'tv_expr) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_comparison_expression ->
    
# 228 "crustyparse.mly"
                 ( CompOp(_1, Lt, _3) )
# 1253 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 227 "crustyparse.mly"
            _3
# 1258 "crustyparse.ml"
   : 'tv_expr) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 227 "crustyparse.mly"
        _2
# 1262 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 227 "crustyparse.mly"
   _1
# 1266 "crustyparse.ml"
   : 'tv_expr) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_comparison_expression ->
    
# 227 "crustyparse.mly"
                  ( CompOp(_1, Neq, _3) )
# 1271 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 226 "crustyparse.mly"
           _3
# 1276 "crustyparse.ml"
   : 'tv_expr) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 226 "crustyparse.mly"
        _2
# 1280 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 226 "crustyparse.mly"
   _1
# 1284 "crustyparse.ml"
   : 'tv_expr) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_comparison_expression ->
    
# 226 "crustyparse.mly"
                 ( CompOp(_1, Eq, _3) )
# 1289 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 178 "crustyparse.mly"
   _1
# 1294 "crustyparse.ml"
   : 'tv_call_args) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_call_args_opt ->
    
# 178 "crustyparse.mly"
              ( _1 )
# 1299 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) : 'tv_call_args_opt ->
    
# 177 "crustyparse.mly"
              ( [] )
# 1305 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 183 "crustyparse.mly"
                          _3
# 1310 "crustyparse.ml"
   : 'tv_call_args) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 183 "crustyparse.mly"
                    _2
# 1314 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 183 "crustyparse.mly"
   _1
# 1318 "crustyparse.ml"
   : 'tv_expr_with_borrow) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_call_args ->
    
# 183 "crustyparse.mly"
                                     ( _1::_3 )
# 1323 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 181 "crustyparse.mly"
          _1
# 1329 "crustyparse.ml"
   : 'tv_expr_with_borrow) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_call_args ->
    
# 182 "crustyparse.mly"
                    ( [_1] )
# 1334 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 206 "crustyparse.mly"
                      _5
# 1339 "crustyparse.ml"
   : 'tv_expr) (_startpos__5_ : Lexing.position) (_endpos__5_ : Lexing.position) (_startofs__5_ : int) (_endofs__5_ : int) (_loc__5_ : Lexing.position * Lexing.position) (
# 206 "crustyparse.mly"
               _4
# 1343 "crustyparse.ml"
   : unit) (_startpos__4_ : Lexing.position) (_endpos__4_ : Lexing.position) (_startofs__4_ : int) (_endofs__4_ : int) (_loc__4_ : Lexing.position * Lexing.position) (
# 206 "crustyparse.mly"
            _3
# 1347 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 1351 "crustyparse.ml"
  )) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 206 "crustyparse.mly"
      _2
# 1355 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 206 "crustyparse.mly"
   _1
# 1359 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 1363 "crustyparse.ml"
  )) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_assignment_expression ->
    
# 206 "crustyparse.mly"
                            ( RefStructAssign(_1, _3, _5) )
# 1368 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 205 "crustyparse.mly"
                    _5
# 1373 "crustyparse.ml"
   : 'tv_expr) (_startpos__5_ : Lexing.position) (_endpos__5_ : Lexing.position) (_startofs__5_ : int) (_endofs__5_ : int) (_loc__5_ : Lexing.position * Lexing.position) (
# 205 "crustyparse.mly"
             _4
# 1377 "crustyparse.ml"
   : unit) (_startpos__4_ : Lexing.position) (_endpos__4_ : Lexing.position) (_startofs__4_ : int) (_endofs__4_ : int) (_loc__4_ : Lexing.position * Lexing.position) (
# 205 "crustyparse.mly"
          _3
# 1381 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 1385 "crustyparse.ml"
  )) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 205 "crustyparse.mly"
      _2
# 1389 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 205 "crustyparse.mly"
   _1
# 1393 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 1397 "crustyparse.ml"
  )) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_assignment_expression ->
    
# 205 "crustyparse.mly"
                          ( StructAssign(_1, _3, _5) )
# 1402 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 204 "crustyparse.mly"
                                        _6
# 1407 "crustyparse.ml"
   : 'tv_expr) (_startpos__6_ : Lexing.position) (_endpos__6_ : Lexing.position) (_startofs__6_ : int) (_endofs__6_ : int) (_loc__6_ : Lexing.position * Lexing.position) (
# 204 "crustyparse.mly"
                                 _5
# 1411 "crustyparse.ml"
   : unit) (_startpos__5_ : Lexing.position) (_endpos__5_ : Lexing.position) (_startofs__5_ : int) (_endofs__5_ : int) (_loc__5_ : Lexing.position * Lexing.position) (
# 204 "crustyparse.mly"
                          _4
# 1415 "crustyparse.ml"
   : unit) (_startpos__4_ : Lexing.position) (_endpos__4_ : Lexing.position) (_startofs__4_ : int) (_endofs__4_ : int) (_loc__4_ : Lexing.position * Lexing.position) (
# 204 "crustyparse.mly"
                  _3
# 1419 "crustyparse.ml"
   : 'tv_id_list) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 204 "crustyparse.mly"
           _2
# 1423 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 204 "crustyparse.mly"
   _1
# 1427 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_assignment_expression ->
    
# 204 "crustyparse.mly"
                                              ( StructExplode(_3, _6) )
# 1432 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 203 "crustyparse.mly"
             _3
# 1437 "crustyparse.ml"
   : 'tv_expr) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 203 "crustyparse.mly"
      _2
# 1441 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 203 "crustyparse.mly"
   _1
# 1445 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 1449 "crustyparse.ml"
  )) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_assignment_expression ->
    
# 203 "crustyparse.mly"
                   ( Assign(_1, _3) )
# 1454 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 223 "crustyparse.mly"
         _2
# 1459 "crustyparse.ml"
   : 'tv_expr) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 223 "crustyparse.mly"
   _1
# 1463 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_arithmetic_expression ->
    
# 223 "crustyparse.mly"
               ( UnArithOp(Neg, _2) )
# 1468 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 222 "crustyparse.mly"
        _2
# 1473 "crustyparse.ml"
   : 'tv_expr) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 222 "crustyparse.mly"
   _1
# 1477 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_arithmetic_expression ->
    
# 222 "crustyparse.mly"
              ( UnArithOp(PostDec, _2) )
# 1482 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 221 "crustyparse.mly"
        _2
# 1487 "crustyparse.ml"
   : 'tv_expr) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 221 "crustyparse.mly"
   _1
# 1491 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_arithmetic_expression ->
    
# 221 "crustyparse.mly"
              ( UnArithOp(PostInc, _2) )
# 1496 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 220 "crustyparse.mly"
        _2
# 1501 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 220 "crustyparse.mly"
   _1
# 1505 "crustyparse.ml"
   : 'tv_expr) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_arithmetic_expression ->
    
# 220 "crustyparse.mly"
              ( UnArithOp(PreDec, _1) )
# 1510 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 219 "crustyparse.mly"
        _2
# 1515 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 219 "crustyparse.mly"
   _1
# 1519 "crustyparse.ml"
   : 'tv_expr) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_arithmetic_expression ->
    
# 219 "crustyparse.mly"
              ( UnArithOp(PreInc, _1) )
# 1524 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 218 "crustyparse.mly"
            _3
# 1529 "crustyparse.ml"
   : 'tv_expr) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 218 "crustyparse.mly"
        _2
# 1533 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 218 "crustyparse.mly"
   _1
# 1537 "crustyparse.ml"
   : 'tv_expr) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_arithmetic_expression ->
    
# 218 "crustyparse.mly"
                  ( ArithOp(_1, Mod, _3) )
# 1542 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 217 "crustyparse.mly"
               _3
# 1547 "crustyparse.ml"
   : 'tv_expr) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 217 "crustyparse.mly"
        _2
# 1551 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 217 "crustyparse.mly"
   _1
# 1555 "crustyparse.ml"
   : 'tv_expr) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_arithmetic_expression ->
    
# 217 "crustyparse.mly"
                     ( ArithOp(_1, Div, _3) )
# 1560 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 216 "crustyparse.mly"
             _3
# 1565 "crustyparse.ml"
   : 'tv_expr) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 216 "crustyparse.mly"
        _2
# 1569 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 216 "crustyparse.mly"
   _1
# 1573 "crustyparse.ml"
   : 'tv_expr) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_arithmetic_expression ->
    
# 216 "crustyparse.mly"
                   ( ArithOp(_1, Mul, _3) )
# 1578 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 215 "crustyparse.mly"
              _3
# 1583 "crustyparse.ml"
   : 'tv_expr) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 215 "crustyparse.mly"
        _2
# 1587 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 215 "crustyparse.mly"
   _1
# 1591 "crustyparse.ml"
   : 'tv_expr) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_arithmetic_expression ->
    
# 215 "crustyparse.mly"
                    ( ArithOp(_1, Sub, _3) )
# 1596 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 214 "crustyparse.mly"
             _3
# 1601 "crustyparse.ml"
   : 'tv_expr) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 214 "crustyparse.mly"
        _2
# 1605 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 214 "crustyparse.mly"
   _1
# 1609 "crustyparse.ml"
   : 'tv_expr) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_arithmetic_expression ->
    
# 214 "crustyparse.mly"
                   ( ArithOp(_1, Add, _3) )
# 1614 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 163 "crustyparse.mly"
   _1
# 1619 "crustyparse.ml"
   : 'tv_args) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_args_opt ->
    
# 163 "crustyparse.mly"
         ( _1 )
# 1624 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) : 'tv_args_opt ->
    
# 162 "crustyparse.mly"
                ( [] )
# 1630 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 168 "crustyparse.mly"
                  _3
# 1635 "crustyparse.ml"
   : 'tv_args) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 168 "crustyparse.mly"
            _2
# 1639 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 168 "crustyparse.mly"
   _1
# 1643 "crustyparse.ml"
   : 'tv_var_decl) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_args ->
    
# 168 "crustyparse.mly"
                        ( _1 :: _3 )
# 1648 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 166 "crustyparse.mly"
      _1
# 1654 "crustyparse.ml"
   : 'tv_var_decl) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_args ->
    
# 167 "crustyparse.mly"
           ( [_1] )
# 1659 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 253 "crustyparse.mly"
                  _4
# 1664 "crustyparse.ml"
   : unit) (_startpos__4_ : Lexing.position) (_endpos__4_ : Lexing.position) (_startofs__4_ : int) (_endofs__4_ : int) (_loc__4_ : Lexing.position * Lexing.position) (
# 253 "crustyparse.mly"
             _3
# 1668 "crustyparse.ml"
   : 'tv_expr) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 253 "crustyparse.mly"
      _2
# 1672 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 253 "crustyparse.mly"
   _1
# 1676 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 1680 "crustyparse.ml"
  )) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_access_expression ->
    
# 253 "crustyparse.mly"
                          ( Index(_1, _3) )
# 1685 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 252 "crustyparse.mly"
        _2
# 1690 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 1694 "crustyparse.ml"
  )) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 252 "crustyparse.mly"
   _1
# 1698 "crustyparse.ml"
   : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_access_expression ->
    
# 252 "crustyparse.mly"
            ( Deref(_2) )
# 1703 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 251 "crustyparse.mly"
            _3
# 1708 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 1712 "crustyparse.ml"
  )) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 251 "crustyparse.mly"
      _2
# 1716 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 251 "crustyparse.mly"
   _1
# 1720 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 1724 "crustyparse.ml"
  )) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_access_expression ->
    
# 251 "crustyparse.mly"
                ( AccessOp(Id(_1), Arrow, _3) )
# 1729 "crustyparse.ml"
     in
  let _ = fun (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_sloc : Lexing.position * Lexing.position) (_loc : Lexing.position * Lexing.position) (
# 250 "crustyparse.mly"
          _3
# 1734 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 1738 "crustyparse.ml"
  )) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (_loc__3_ : Lexing.position * Lexing.position) (
# 250 "crustyparse.mly"
      _2
# 1742 "crustyparse.ml"
   : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_loc__2_ : Lexing.position * Lexing.position) (
# 250 "crustyparse.mly"
   _1
# 1746 "crustyparse.ml"
   : (
# 27 "crustyparse.mly"
       (string)
# 1750 "crustyparse.ml"
  )) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) (_loc__1_ : Lexing.position * Lexing.position) : 'tv_access_expression ->
    
# 250 "crustyparse.mly"
              ( AccessOp(Id(_1), Dot, _3) )
# 1755 "crustyparse.ml"
     in
  ((let rec diverge() = diverge() in diverge()) : 'tv_var_decl_list * 'tv_var_decl * 'tv_struct_decl * 'tv_stmt_list * 'tv_stmt * 'tv_single_type * 'tv_return_stmt * 'tv_program * 'tv_primtype * 'tv_logical_expression * 'tv_literal_expression * 'tv_lin_qual * 'tv_ifstmt * 'tv_id_list * 'tv_func_decl * 'tv_exprs_list * 'tv_expr_with_borrow * 'tv_expr * 'tv_decls * 'tv_comparison_expression * 'tv_call_args_opt * 'tv_call_args * 'tv_assignment_expression * 'tv_arithmetic_expression * 'tv_args_opt * 'tv_args * 'tv_access_expression)

and menhir_end_marker =
  0
