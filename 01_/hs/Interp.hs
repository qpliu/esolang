module Interp(apply,arity) where

import Data.List((!!))
import Data.Map(Map,(!))

import SyntaxTree(Bits,Pattern(Literal,Binding,Wildcard),Expr(LiteralBits,Concat,Bound,Call),Definition(Def))

apply :: Map String [Definition] -> String -> [Bits] -> Bits
arity :: Map String [Definition] -> String -> Int

arity defs fn = case defs ! fn of (Def patterns _):_ -> length patterns

apply defs fn args =
    let evalMatching Nothing (Def patterns expr) =
            case bind patterns args [] of
                Just bindings -> Just (eval defs bindings expr)
                _ -> Nothing
        evalMatching result _ = result
    in  case foldl evalMatching Nothing (defs ! fn) of
            Just bits -> bits
            _ -> error ("No matching pattern for "++fn)

bind :: [Pattern] -> [Bits] -> [Bits] -> Maybe [Bits]
bind (pattern:patterns) (arg:args) acc =
    case pattern of
        Literal bits ->
            case matchArg bits arg of
                Just [] -> bind patterns args acc
                _ -> Nothing
        Binding bits _ ->                
            case matchArg bits arg of
                Just binding -> bind patterns args (binding:acc)
                _ -> Nothing
        Wildcard bits ->                
            case matchArg bits arg of
                Just _ -> bind patterns args acc
                _ -> Nothing
bind [] [] acc = Just (reverse acc)

matchArg (bit:bits) (argBit:argBits)
    | bit == argBit = matchArg bits argBits
    | otherwise     = Nothing
matchArg [] arg = Just arg
matchArg _ [] = Nothing

eval :: Map String [Definition] -> [Bits] -> Expr -> Bits
eval _ _ (LiteralBits bits) = bits
eval defs bindings (Concat head tail) =
    eval defs bindings head ++ eval defs bindings tail
eval _ bindings (Bound i) = bindings !! i
eval defs bindings (Call fn args) =
    apply defs fn (map (eval defs bindings) args)
