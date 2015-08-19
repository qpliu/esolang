module Parse(Def(..),Expr(..),Fn,Pattern(..),arity,parse)
where

import Control.Monad(foldM)
import Data.Char(isSpace)
import Data.Either(lefts,rights)
import Data.Map(Map,empty,insert,toList,(!))
import qualified Data.Map as M

type Fn = [Def]
data Def = Def [Pattern] Expr
data Pattern = PatternBound Int | PatternLiteral Int | PatternIgnore Int
data Expr = ExprLiteral Int
          | ExprBound Int
          | ExprConcat Expr Expr
          | ExprFuncall Fn [Expr]

parse :: String -> Either String (Map String Fn)
parse str = pass1 (tokenize str) >>= pass2

arity :: Fn -> Int
arity (Def patterns _:_) = length patterns

data Token = Colon | Nil | Dot | Eq | Symbol String
  deriving (Eq,Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('=':'=':str) = tokenize (dropWhile (/= '\n') str)
tokenize (':':str) = Colon : tokenize str
tokenize ('_':str) = Nil : tokenize str
tokenize ('.':str) = Dot : tokenize str
tokenize ('=':str) = Eq : tokenize str
tokenize (c:str)
  | isSpace c = tokenize str
  | otherwise = Symbol (c:sym) : tokenize rest
      where
        (sym,rest) = span symChar str
        symChar c = not (isSpace c) && not (c `elem` ":_.=")

type Fn1 = [Def1]
data Def1 = Def1 [Pattern1] [Token]
  deriving Show
data Pattern1 =
    Pattern1Bound Int String | Pattern1Literal Int | Pattern1Ignore Int
  deriving Show

boundNames :: [Pattern1] -> [String]
boundNames patterns = concatMap boundName patterns
  where
    boundName (Pattern1Bound _ name) = [name]
    boundName _ = []

pass1 :: [Token] -> Either String (Map String Fn1)
pass1 toks = parse1 toks empty
  where
    parse1 [] fns = Right fns
    parse1 (Symbol name:toks) fns = parseDef name [] 0 toks fns
    parse1 (Dot:toks) fns = parse1 toks fns
    parse1 (Colon:_) fns = Left "Unexpected :"
    parse1 (Nil:_) fns = Left "Unexpected _"
    parse1 (Eq:_) fns = Left "Unexpected ="
    parseDef name patterns colons (Colon:toks) fns =
        parseDef name patterns (colons+1) toks fns
    parseDef name patterns 0 (Eq:toks) fns =
        let (body,rest) = span bodyTok toks
            bodyTok tok = tok /= Dot && tok /= Eq
            oldDefs = M.lookup name fns
            arity1 = maybe (length patterns)
                          (\ (Def1 pats _:_) -> length pats)
                          oldDefs
            newDef = Def1 (reverse patterns) body
            newDefs = maybe [newDef] (++[newDef]) oldDefs
        in  if arity1 /= length patterns
              then Left ("Arity mismatch: " ++ show (length patterns)
                         ++ ", previously defined with " ++ show arity1 ++ ": "
                         ++ name)
              else if null rest
                     then Left "Unexpected EOF"
                     else parse1 rest (insert name newDefs fns)
    parseDef name patterns colons toks@(Eq:_) fns =
        parseDef name (Pattern1Ignore colons:patterns) 0 toks fns
    parseDef name patterns colons (Nil:toks) fns =
        parseDef name (Pattern1Literal colons:patterns) 0 toks fns
    parseDef name patterns colons (Dot:toks) fns =
        parseDef name (Pattern1Ignore colons:patterns) 0 toks fns
    parseDef name patterns colons (Symbol param:toks) fns =
        if param `elem` boundNames patterns
          then Left ("Duplicate parameter: " ++ param ++ " in: " ++ name)
          else parseDef name (Pattern1Bound colons param:patterns) 0 toks fns

pass2 :: Map String Fn1 -> Either String (Map String Fn)
pass2 fns1 = result
  where
    arity1 name = fmap (\ (Def1 pats _:_) -> length pats) (M.lookup name fns1)
    Right finalFns = result
    result = foldM parseFn empty (toList fns1)
    parseFn :: Map String Fn -> (String,Fn1) -> Either String (Map String Fn)
    parseFn fns (name,defs1) =
        if null (lefts defs)
          then Right (insert name (rights defs) fns)
          else Left (head (lefts defs))
      where
        defs = map parseDef defs1
    parseDef :: Def1 -> Either String Def
    parseDef (Def1 patterns toks) =
        if null (lefts exprs)
          then if null (rights exprs)
                 then Left "Empty body"
                 else Right (Def (map parsePat1 patterns) (concatExprs (rights exprs)))
          else Left (head (lefts exprs))
      where
        exprs = parseExprs (boundNames patterns) toks
        parsePat1 (Pattern1Bound n _) = PatternBound n
        parsePat1 (Pattern1Literal n) = PatternLiteral n
        parsePat1 (Pattern1Ignore n) = PatternIgnore n
        concatExprs [expr] = expr
        concatExprs (expr:exprs) = ExprConcat expr (concatExprs exprs)
    parseExprs :: [String] -> [Token] -> [Either String Expr]
    parseExprs params [] = []
    parseExprs params (Colon:toks) =
        Right (ExprLiteral (1 + length colons)) : parseExprs params rest
      where
        (colons,afterColons) = span (== Colon) toks
        rest | [Nil] == take 1 afterColons = drop 1 afterColons
             | otherwise = afterColons
    parseExprs params (Nil:toks) =
        Right (ExprLiteral 0) : parseExprs params toks
    parseExprs params (Dot:toks) = [Left "Unexpected ."]
    parseExprs params (Eq:toks) = [Left "Unexpected ="]
    parseExprs params (Symbol sym:toks)
      | sym `elem` params =
            let Just i = lookup sym (zip params [0..])
            in  Right (ExprBound i) : parseExprs params toks
      | otherwise =
            maybe [Left ("Undefined symbol: " ++ sym)]
                (parseFuncall params sym toks (finalFns ! sym)) (arity1 sym)
    parseFuncall :: [String] -> String -> [Token] -> Fn -> Int -> [Either String Expr]
    parseFuncall params fnName toks fn nparams =
        if null (lefts args)
          then if length args == nparams
                 then Right (ExprFuncall fn (rights args)) : rest
                 else [Left ("Not enough arguments provided to: " ++ fnName)]
          else [Left (head (lefts args))]
      where
        (args,rest) = splitAt nparams (parseExprs params toks)
