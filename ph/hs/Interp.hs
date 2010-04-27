module Interp(rootScope,eval) where

import Prelude hiding (lookup)
import Data.Ix(Ix)
import Value(Value(Nil,Cons),isNil,car,cdr)
import Scope(Scope,root,nested,get)
import ValueTrie(empty,insert,lookup)

data Key = Head | Tail deriving (Bounded, Eq, Ix, Ord)
key :: Value -> [Key]
key Nil = []
key (Cons head tail) = Head : key head ++ Tail : key tail

data Op = Op (Value -> Scope Value Op -> Value -> Value)
op (Op op) = op

rootScope :: Scope Value Op
rootScope = root getBinding where
    getBinding Nil = Just (Op quoteOp)
    getBinding (Cons Nil Nil) = Just (Op letOp)
    getBinding (Cons Nil (Cons Nil Nil)) = Just (Op cdrOp)
    getBinding (Cons Nil (Cons Nil (Cons Nil Nil))) = Just (Op ifOp)
    getBinding (Cons Nil (Cons (Cons Nil Nil) Nil)) = Just (Op concatOp)
    getBinding (Cons (Cons Nil Nil) Nil) = Just (Op carOp)
    getBinding (Cons (Cons Nil Nil) (Cons Nil Nil)) = Just (Op consOp)
    getBinding (Cons (Cons (Cons Nil Nil) Nil) Nil) = Just (Op evalOp)
    getBinding _ = Nothing

quoteOp input scope arg = arg

letOp input scope Nil = Nil
letOp input scope (Cons bindings letBody) =
    let bindingList Nil = empty
        bindingList (Cons Nil rest) = bindingList rest
        bindingList (Cons (Cons name body) rest) =
            let definedOp input scope arg =
                    eval (eval input scope arg) letScope body
            in  insert name (Op definedOp) (bindingList rest)
        letScope = nested scope (flip lookup (bindingList bindings))
    in  eval input letScope letBody

cdrOp input scope arg = cdr (eval input scope arg)

ifOp input scope Nil = Nil
ifOp input scope (Cons _ Nil) = Nil
ifOp input scope (Cons cond body)
    | isNil (eval input scope cond) = eval input scope (cdr body)
    | otherwise                     = eval input scope (car body)

carOp input scope arg = car (eval input scope arg)

consOp input scope Nil = Nil
consOp input scope (Cons head tail) =
    Cons (eval input scope head) (eval input scope tail)

evalOp input scope arg = eval input scope (eval input scope arg)

concatOp input scope Nil = Nil
concatOp input scope (Cons head tail) =
    let concat Nil rest = rest
        concat (Cons Nil Nil) rest = rest
        concat (Cons Nil tl) rest = Cons Nil (concat tl rest)
        concat (Cons hd tl) rest = Cons (concat hd rest) tl
    in  concat (eval input scope head) (eval input scope tail)

eval input scope Nil = input
eval input scope expr = (op (get scope (car expr))) input scope (cdr expr)
