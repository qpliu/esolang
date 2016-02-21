module Eval
    (eval)
where

import Ast(Def(..),Expr(..))
import Value(Value,union,intersect,diff,fromList,toList,combinations)

eval :: [Value] -> Expr -> Value
eval bindings expr = e expr
  where
    e (ExprBag _ elts) = fromList (map (fmap (eval bindings)) elts)
    e (ExprBound _ _ index) = bindings !! index
    e (ExprUnion _ lhs rhs) = union (eval bindings lhs) (eval bindings rhs)
    e (ExprIntersect _ lhs rhs) =
        intersect (eval bindings lhs) (eval bindings rhs)
    e (ExprDiff _ lhs rhs) = diff (eval bindings lhs) (eval bindings rhs)
    e (ExprFuncall _ (Def _ _ expr) args)
      | or (map fst args) =
            fromList (map (evalFuncall expr) (combinations (map evalArg args)))
      | otherwise = eval (map (eval bindings . snd) args) expr
    evalArg (False,expr) = [(1,eval bindings expr)]
    evalArg (True,expr) = toList (eval bindings expr)
    evalFuncall expr args = (product (map fst args),eval (map snd args) expr)
