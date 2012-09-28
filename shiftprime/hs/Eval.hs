module Eval(eval,evalFuncall,fromVal,toVal) where

import Data.Either(partitionEithers)

import Parse(Def(Def),DefBody(DefBody),DefGuard(DefGuard),
             Expr(ExprNumber,ExprParam,ExprFuncall,ExprDiv,ExprRem,ExprMul,
                  ExprCompileError))
import Tokenize(Location(Location))

primes :: [Integer]
primes = 2:[x | x <- [3,5..], and [x `mod` y /= 0 | y <- takeWhile (\ y -> y*y <= x) primes]]

-- FIXME: One problem with this representation is that factors of 1 in the
-- nonzero number of factors of primes is lost.
-- %%((2,0),0) is 1, but should be 2.

toVal :: Integer -> [Integer]
toVal 0 = []
toVal 1 = [1]
toVal x = 0:toVal' x 0 primes
  where
    toVal' 1 factors _ = [factors]
    toVal' n factors (p:ps) =
        case n `divMod` p of
          (n',0) -> toVal' n' (factors+1) (p:ps)
          _ -> factors:toVal' n 0 ps

fromVal :: [Integer] -> Integer
fromVal [] = 0
fromVal (f:fs) = fromVal' (f == 0) 1 fs primes
  where
    fromVal' True 1 [] _ = 0
    fromVal' _ v [] _ = v
    fromVal' maybeZero n (f:fs) (p:ps) =
        fromVal' (maybeZero && f == 0) (n*p^f) fs ps

eq :: [Integer] -> [Integer] -> Bool
eq [] [] = True
eq [] (0:v2) = eq' [] v2
eq [] _ = False
eq (0:v1) [] = eq' v1 []
eq _ [] = False
eq (_:v1) (_:v2) = eq' v1 v2

eq' :: [Integer] -> [Integer] -> Bool
eq' [] [] = True
eq' [] (0:v2) = eq' [] v2
eq' [] _ = False
eq' (0:v1) [] = eq' v1 []
eq' _ [] = False
eq' (v1:v1s) (v2:v2s)
  | v1 == v2 = eq' v1s v2s
  | otherwise = False

eval :: (String -> Maybe Def) -> Expr -> Either (Location,String) [Integer]
eval defs expr = eval' defs [] expr

eval' :: (String -> Maybe Def) -> [[Integer]] -> Expr -> Either (Location,String) [Integer]
eval' defs args (ExprNumber number) = Right (toVal number)
eval' defs args (ExprParam i) = Right (args !! i)
eval' defs args (ExprFuncall f location exprs) =
    case (defs f,partitionEithers (map (eval' defs args) exprs)) of
      (Just def,([],args')) -> evalFuncall defs args' def
      (Nothing,_) -> Left (location,"This shouldn't happen:Undefined symbol:"++f)
      (_,(err:_,_)) -> Left err
eval' defs args (ExprDiv expr) =
    either Left (Right . drop 1) (eval' defs args expr)
eval' defs args (ExprRem expr) =
    case eval' defs args expr of
      Left err -> Left err
      Right [] -> Right []
      Right (n:_) -> Right (toVal n)
eval' defs args (ExprMul leftExpr rightExpr) =
    case (eval' defs args leftExpr,eval' defs args rightExpr) of
      (Left err,_) -> Left err
      (_,Left err) -> Left err
      (Right leftVal,Right rightVal) -> Right (fromVal leftVal:rightVal)
eval' defs args (ExprCompileError err) = Left err

evalFuncall :: (String -> Maybe Def) -> [[Integer]] -> Def -> Either (Location,String) [Integer]
evalFuncall defs args (Def location name _ defBodies) = evalDefBody defBodies
  where
    evalDefBody [] = Left (location,"This shouldn't happen:No matching body:"++name)
    evalDefBody (DefBody expr guards:bodies) =
      case testGuards guards of
        Left err -> Left err
        Right True -> eval' defs args expr
        _ -> evalDefBody bodies
    testGuards [] = Right True
    testGuards (DefGuard expr1 expr2:guards) =
      case (eval' defs args expr1,eval' defs args expr2) of
        (Left err,_) -> Left err
        (_,Left err) -> Left err
        (Right val1,Right val2) ->
            if val1 `eq` val2 then testGuards guards else Right False
