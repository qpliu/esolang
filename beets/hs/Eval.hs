module Eval(eval)
where

import Ast
       (Function(Function),
       Expr(ExprParam,ExprCall,ExprTree,ExprLeft,ExprRight,ExprCond))
import Tree(Tree,cond,cons,left,right)

eval :: [Tree] -> Expr -> Tree
eval args (ExprParam _ i) = args !! i
eval args (ExprCall _ (Function _ _ expr) params) =
    eval (map (eval args) params) expr
eval args (ExprTree _ val lexpr rexpr) =
    cons val (eval args lexpr) (eval args rexpr)
eval args (ExprLeft _ expr) = left (eval args expr)
eval args (ExprRight _ expr) = right (eval args expr)
eval args (ExprCond _ test fexpr texpr) =
    cond (eval args test) (eval args fexpr) (eval args texpr)
