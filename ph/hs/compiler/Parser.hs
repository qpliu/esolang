module Parser(Expr(Arg,Quote,Let,CarFn,CdrFn,ConsFn,If,Call),parse) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Reader(Value(Cons,Nil),foldValue)

data Expr = Arg
          | Quote Value
          | Let (Map.Map Value Expr) Expr
          | CarFn Expr
          | CdrFn Expr
          | ConsFn Expr Expr
          | If Expr Expr Expr
          | Call Value Expr
    deriving Show

parse :: Value -> Expr
parse value = parseInScope value Set.empty

parseInScope :: Value -> Set.Set Value -> Expr
parseInScope Nil _ = Arg
parseInScope value@(Cons fn arg) scope
  | Set.member fn scope = Call fn (parseInScope arg scope)
  | otherwise = parseIntrinsic value scope

parseIntrinsic :: Value -> Set.Set Value -> Expr
parseIntrinsic Nil _ = Arg
parseIntrinsic (Cons Nil value) _ = Quote value
parseIntrinsic (Cons (Cons Nil Nil) (Cons bindings body)) scope =
    let newScope = foldValue addBoundName scope bindings
        addBoundName set Nil = set
        addBoundName set (Cons name _) = Set.insert name set
        parsedBindings = foldValue addBinding Map.empty bindings
        addBinding bindings Nil = bindings
        addBinding bindings (Cons name body) =
            Map.insert name (parseInScope body newScope) bindings
    in  Let parsedBindings (parseInScope body newScope)
parseIntrinsic (Cons (Cons (Cons Nil Nil) Nil) arg) scope =
    CarFn (parseInScope arg scope)
parseIntrinsic (Cons (Cons Nil (Cons Nil Nil)) arg) scope =
    CdrFn (parseInScope arg scope)
parseIntrinsic (Cons (Cons (Cons Nil Nil) (Cons Nil Nil)) Nil) scope =
    Quote Nil
parseIntrinsic (Cons (Cons (Cons Nil Nil) (Cons Nil Nil)) (Cons head tail))
               scope =
    ConsFn (parseInScope head scope) (parseInScope tail scope)
parseIntrinsic (Cons (Cons Nil (Cons Nil (Cons Nil Nil))) Nil) scope =
    Quote Nil
parseIntrinsic (Cons (Cons Nil (Cons Nil (Cons Nil Nil))) (Cons _ Nil))
               scope =
    Quote Nil
parseIntrinsic (Cons (Cons Nil (Cons Nil (Cons Nil Nil)))
                     (Cons cond (Cons ifTrue ifFalse)))
               scope =
    If (parseInScope cond scope)
       (parseInScope ifTrue scope)
       (parseInScope ifFalse scope)
parseIntrinsic _ _ = error "parse"
