module Compiler(compile) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Parser
    (Defn(Defn),
     Expr(ExprLiteral,ExprParam,ExprFuncall),
     Param(ParamBound,ParamLiteral,ParamWild),
     arity)

compile :: String -> Bool -> Map.Map String [Defn] -> String
compile mainFunction debug fns =
    maybe (error ("No such function: " ++ mainFunction))
          (unlines . compile') (arity fns mainFunction)
  where
    compile' :: Int -> [String]
    compile' mainArity =
          compileConstants fns
       ++ compileFns fns
       ++ compileMain mainFunction mainArity debug

constantName :: [Bool] -> String
constantName bits = "@L" ++ map (\ b -> if b then '1' else '0') bits

compileConstants :: Map.Map String [Defn] -> [String]
compileConstants fns = map compileConstant (constants fns)

compileConstant :: [Bool] -> String
compileConstant bits =
    constantName bits ++ " = linkonce_odr constant ["
        ++ show (length bits) ++ " x i1] ["
        ++ drop 1 (concatMap (\ b -> ",i1 " ++ if b then "1" else "0") bits)
        ++ "]"

constants :: Map.Map String [Defn] -> [[Bool]]
constants fns =
    Set.toList (Map.fold (flip (foldl defnConstants)) Set.empty fns)

defnConstants :: Set.Set [Bool] -> Defn -> Set.Set [Bool]
defnConstants set (Defn _ exprs) = foldl exprConstants set exprs

exprConstants :: Set.Set [Bool] -> Expr -> Set.Set [Bool]
exprConstants set (ExprLiteral bits) = Set.insert bits set
exprConstants set (ExprParam _) = set
exprConstants set (ExprFuncall _ exprs) = foldl exprConstants set exprs

compileFns :: Map.Map String [Defn] -> [String]
compileFns fns = concatMap (uncurry compileFn) (Map.assocs fns)

compileFn :: String -> [Defn] -> [String]
compileFn name defns = []

compileMain :: String -> Int -> Bool -> [String]
compileMain mainFunction mainArity debug = []
