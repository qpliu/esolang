module Flattener(Expr(Arg,Quote,CarFn,CdrFn,ConsFn,If,Call),flatten) where

import qualified Data.Map as Map

import qualified Parser as P
import Reader(Value)

data Expr = Arg
          | Quote Value
          | CarFn Expr
          | CdrFn Expr
          | ConsFn Expr Expr
          | If Expr Expr Expr
          | Call Int Expr
    deriving Show

flatten :: P.Expr -> (Map.Map Int (Value,Expr),Expr)
flatten expr = flattenInScope expr Map.empty Map.empty

flattenInScope :: P.Expr -> Map.Map Int (Value,Expr) -> Map.Map Value Int
                         -> (Map.Map Int (Value,Expr),Expr)
flattenInScope P.Arg flattened scope = (flattened,Arg)
flattenInScope (P.Quote value) flattened scope = (flattened,Quote value)
flattenInScope (P.Let bindings expr) flattened scope =
    let scope1 = foldl (flip (uncurry Map.insert)) scope
                       (zip (Map.keys bindings) [Map.size flattened..])
        flattened1 = foldl (flip (uncurry Map.insert)) flattened
                           (zip [Map.size flattened..]
                                (map (flip (,) Arg) (Map.keys bindings)))
        flattened2 = Map.foldWithKey addBinding flattened1 bindings
        addBinding name bindingExpr flattened3 =
            let (flattened4,bindingExpr4) =
                    flattenInScope bindingExpr flattened3 scope1
            in  Map.insert ((Map.!) scope1 name) (name,bindingExpr4) flattened4
    in  flattenInScope expr flattened2 scope1
flattenInScope (P.CarFn argExpr) flattened scope =
    let (flattened1,arg) = flattenInScope argExpr flattened scope
    in  (flattened1,CarFn arg)
flattenInScope (P.CdrFn argExpr) flattened scope =
    let (flattened1,arg) = flattenInScope argExpr flattened scope
    in  (flattened1,CdrFn arg)
flattenInScope (P.ConsFn headExpr tailExpr) flattened scope =
    let (flattened1,head) = flattenInScope headExpr flattened scope
        (flattened2,tail) = flattenInScope tailExpr flattened1 scope
    in  (flattened2,ConsFn head tail)
flattenInScope (P.If condExpr ifTrueExpr ifFalseExpr) flattened scope =
    let (flattened1,cond) = flattenInScope condExpr flattened scope
        (flattened2,ifTrue) = flattenInScope ifTrueExpr flattened1 scope
        (flattened3,ifFalse) = flattenInScope ifFalseExpr flattened2 scope
    in  (flattened3,If cond ifTrue ifFalse)
flattenInScope (P.Call name argExpr) flattened scope =
    let (flattened1,arg) = flattenInScope argExpr flattened scope
    in  (flattened1,Call ((Map.!) scope name) arg)
