module ParseBodies(Bits,Pattern(Literal,Binding,Wildcard),Definition(Def),parseBodies) where

import Data.List(elemIndex)
import Data.Map(Map,empty,findWithDefault,insert,member,(!))
import qualified Data.Map(map)

import Tokenizer(Token(Symbol,Zero,One,Equal,Dot,Nil))
import SyntaxTree(Bits,Pattern(Literal,Binding,Wildcard),Expr(LiteralBits,Concat,Bound,Call),Definition(Def))
import ParseSigs(Signature(Sig))

parseBodies :: [Signature] -> Map String [Definition]

groupDefs [] defs = Data.Map.map reverse defs
groupDefs (sig@(Sig name patterns _):sigs) defs =
    case findWithDefault [] name defs of
        [] -> groupDefs sigs (insert name [sig] defs)
        siglist@(Sig _ otherPatterns _:_) ->
            if length patterns /= length otherPatterns then
                error ("arity mismatch:"++name)
            else
                groupDefs sigs (insert name (sig:siglist) defs)

parseDefs defs = Data.Map.map (map (parseDef arities)) defs
    where
        arities = Data.Map.map (\ (Sig _ patterns _:_) -> length patterns) defs

parseDef arities (Sig _ patterns body) =
    case body of
        [] -> Def patterns (LiteralBits [])
        _ -> Def patterns
            (foldr1 (\a b -> Concat a b) (parseExprs arities bindings body))
    where
        bindings = map (\ (Binding _ name) -> name)
            (filter (\x -> case x of Binding _ _ -> True; _ -> False) patterns)

parseExprs arities bindings [] = []
parseExprs arities bindings tokens@(Zero:_) =
    parseLiteral arities bindings tokens []
parseExprs arities bindings tokens@(One:_) =
    parseLiteral arities bindings tokens []
parseExprs arities bindings tokens@(Nil:_) =
    parseLiteral arities bindings tokens []
parseExprs arities bindings (Symbol name:tokens) =
    case elemIndex name bindings of
        Just i -> Bound i : parseExprs arities bindings tokens
        Nothing ->
            let (args,exprs) = splitAt (arities ! name) (parseExprs arities bindings tokens)
            in Call name args : exprs

parseLiteral arities bindings (Zero:tokens) literal =
    parseLiteral arities bindings tokens (False:literal)
parseLiteral arities bindings (One:tokens) literal =
    parseLiteral arities bindings tokens (True:literal)
parseLiteral arities bindings [] literal = [LiteralBits (reverse literal)]
parseLiteral arities bindings [Nil] literal = [LiteralBits (reverse literal)]
parseLiteral arities bindings (Nil:tokens) literal =
    LiteralBits (reverse literal) : parseExprs arities bindings tokens
parseLiteral arities bindings tokens literal =
    LiteralBits (reverse literal) : parseExprs arities bindings tokens

parseBodies sigs = parseDefs (groupDefs sigs empty)
