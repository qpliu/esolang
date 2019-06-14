module Ast
       (Function(Function),
       Expr(ExprParam,ExprCall,ExprTree,ExprLeft,ExprRight,ExprCond))
where

import Tokenize(Token)

data Function = Function Token [Token] Expr

data Expr = ExprParam Token Int
     | ExprCall Token Function [Expr]
     | ExprTree Token Bool Expr Expr
     | ExprLeft Token Expr
     | ExprRight Token Expr
     | ExprCond Token Expr Expr Expr
