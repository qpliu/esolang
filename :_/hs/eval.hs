module Eval(Val(..),eval,evalCall,numToVal,valToNum)
where

import Parse(Def(..),Expr(..),Fn,Pattern(..))

data Val = Ptr Val | Nil | Concat Val Val

numToVal :: Integer -> Val
numToVal x | x > 0 = Ptr (numToVal (x - 1)) | otherwise = Nil

valToNum :: Val -> Integer
valToNum x = toNum 0 x
  where
    toNum n Nil = n
    toNum n (Ptr x) = toNum (n+1) x
    toNum n (Concat v1 v2) = toNum (toNum n v1) v2

vnil :: Val -> Bool
vnil Nil = True
vnil (Concat v1 v2) = vnil v1 && vnil v2
vnil _ = False

vtail :: Val -> Int -> Maybe Val
vtail v 0 = Just v
vtail (Ptr v) n = vtail v (n-1)
vtail (Concat Nil v) n = vtail v n
vtail (Concat (Ptr Nil) v) n = vtail v (n-1)
vtail (Concat (Ptr v1) v2) n = vtail (Concat v1 v2) (n-1)
vtail Nil _ = Nothing

eval :: [Val] -> Expr -> Val
eval bindings (ExprLiteral n) = numToVal (fromIntegral n)
eval bindings (ExprBound i) = bindings !! i
eval bindings (ExprConcat e1 e2) = Concat (eval bindings e1) (eval bindings e2)
eval bindings (ExprFuncall fn exprs) = evalCall fn (map (eval bindings) exprs)

evalCall :: Fn -> [Val] -> Val
evalCall [] args = error "evalCall failed: No matching definition"
evalCall (Def patterns expr:defs) args =
    maybe (evalCall defs args) (flip eval expr) (matchPats patterns args [])
  where
    matchPats [] [] bindings = Just (reverse bindings)
    matchPats [] _ _ = Nothing
    matchPats _ [] _ = Nothing
    matchPats (PatternBound n:pats) (val:vals) bindings =
        maybe Nothing (matchPats pats vals . (:bindings)) (vtail val n)
    matchPats (PatternLiteral n:pats) (val:vals) bindings =
        maybe Nothing
             (\ v -> if vnil v then matchPats pats vals bindings else Nothing)
             (vtail val n)
    matchPats (PatternIgnore n:pats) (val:vals) bindings =
        maybe Nothing (const (matchPats pats vals bindings)) (vtail val n)
