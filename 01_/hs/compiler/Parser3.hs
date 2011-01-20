module Parser3
    (Defn(Defn),
     Expr(ExprLiteral,ExprParam,ExprFuncall),
     Param(ParamBound,ParamLiteral,ParamWild),
     parser3)
where

import Data.Map(Map,empty,foldWithKey,insert)
import qualified Data.Map
import Text.ParserCombinators.Parsec
    (GenParser,ParseError,SourcePos,count,eof,many,parse,token,(<|>),(<?>))
import Text.ParserCombinators.Parsec.Error(Message(Message),newErrorMessage)

import Parser1
    (Defn1(Defn1),
     Expr1(Expr1Literal,Expr1Symbol),
     Param1(Param1Bound,Param1Literal,Param1Wild))

-- Parser pass 3.

data Defn = Defn [Param] [Expr]
    deriving Show

data Param = ParamBound [Bool] | ParamLiteral [Bool] | ParamWild [Bool]
    deriving Show

data Expr = ExprLiteral [Bool] | ExprParam Int | ExprFuncall String [Expr]
    deriving Show

parser3 :: Map String [Defn1] -> Either ParseError (Map String [Defn])
parser3 defn1s =
    foldWithKey (parseDefns (getArity defn1s)) (Right empty) defn1s

getArity :: Map String [Defn1] -> String -> Maybe Int
getArity defn1s name = fmap (arity . head) (Data.Map.lookup name defn1s)
  where
    arity (Defn1 _ _ params _) = length params

parseDefns :: (String -> Maybe Int) -> String -> [Defn1]
                                    -> Either ParseError (Map String [Defn])
                                    -> Either ParseError (Map String [Defn])
parseDefns arity name defn1s defns =
    either Left (flip fmap defns . insert name)
           (concatRight (map (parseDefn arity) defn1s))

parseDefn :: (String -> Maybe Int) -> Defn1 -> Either ParseError Defn
parseDefn arity (Defn1 _ _ param1s expr1s) =
    apply (flip (flip parse "") expr1s . parseBody arity)
          (parse parseParams "" param1s)

parseParams :: GenParser Param1 () ([Param],[String])
parseParams = do
    (params,names) <- parseParamList ([],[])
    return (reverse params,reverse names)

parseParamList :: ([Param],[String]) -> GenParser Param1 () ([Param],[String])
parseParamList list =
    (eof >> return list) <|> addBound list <|> addLiteral list <|> addWild list
                         <?> "Duplicate parameter name"

addBound :: ([Param],[String]) -> GenParser Param1 () ([Param],[String])
addBound list@(params,names) = do
    (bits,name) <- param1Token getBound
    if name `elem` names
        then fail "Duplicate parameter name"
        else parseParamList ((ParamBound bits):params,name:names)
  where
    getBound (Param1Bound _ bits name) = Just (bits,name)
    getBound _ = Nothing

addLiteral :: ([Param],[String]) -> GenParser Param1 () ([Param],[String])
addLiteral list@(params,names) = do
    bits <- param1Token getLiteral
    parseParamList (ParamLiteral bits:params,names)
  where
    getLiteral (Param1Literal _ bits) = Just bits
    getLiteral _ = Nothing

addWild :: ([Param],[String]) -> GenParser Param1 () ([Param],[String])
addWild list@(params,names) = do
    bits <- param1Token getWild
    parseParamList (ParamWild bits:params,names)
  where
    getWild (Param1Wild _ bits) = Just bits
    getWild _ = Nothing

param1Token :: (Param1 -> Maybe a) -> GenParser Param1 () a
param1Token test = token showToken getPosition test
  where
    showToken (Param1Bound _ bits name) = showBits bits ++ name
    showToken (Param1Literal _ bits) = showBits bits ++ "_"
    showToken (Param1Wild _ bits) = showBits bits ++ "."
    getPosition (Param1Bound sourcePos _ _) = sourcePos
    getPosition (Param1Literal sourcePos _) = sourcePos
    getPosition (Param1Wild sourcePos _) = sourcePos

showBits :: [Bool] -> String
showBits bits = map (\ b -> if b then '1' else '0') bits

parseBody :: (String -> Maybe Int) -> ([Param],[String])
                                   -> GenParser Expr1 () Defn
parseBody arity (params,paramNames) = do
    exprs <- many (parseExpr arity
                             (flip lookup (zip paramNames
                                               (map ExprParam [0..]))))
    eof <?> "Undefined symbol or not enough arguments to a function."
    return (Defn params exprs)

parseExpr :: (String -> Maybe Int) -> (String -> Maybe Expr)
                                   -> GenParser Expr1 () Expr
parseExpr arity boundParameter =
    literalExpr <|> paramExpr boundParameter
                <|> funcallExpr arity boundParameter

expr1Token :: (Expr1 -> Maybe a) -> GenParser Expr1 () a
expr1Token test = token showToken getPosition test
  where
    showToken (Expr1Literal _ bits) = showBits bits
    showToken (Expr1Symbol _ name) = name
    getPosition (Expr1Literal sourcePos _) = sourcePos
    getPosition (Expr1Symbol sourcePos _) = sourcePos

literalExpr :: GenParser Expr1 () Expr
literalExpr = expr1Token getLiteral
  where
    getLiteral (Expr1Literal _ bits) = Just (ExprLiteral bits)
    getLiteral _ = Nothing

paramExpr :: (String -> Maybe Expr) -> GenParser Expr1 () Expr
paramExpr boundParameter = expr1Token getParameter
  where
    getParameter (Expr1Symbol _ name) = boundParameter name
    getParameter _ = Nothing

funcallExpr :: (String -> Maybe Int) -> (String -> Maybe Expr)
                                     -> GenParser Expr1 () Expr
funcallExpr arity boundParameter = do
    (name,argc) <- expr1Token getFunction
    args <- count argc (parseExpr arity boundParameter)
    return (ExprFuncall name args)
  where
    getFunction (Expr1Symbol _ name) = fmap ((,) name) (arity name)
    getFunction _ = Nothing
    

apply :: (a -> Either b c) -> Either b a -> Either b c
apply f e = either Left f e

concatRight :: [Either a b] -> Either a [b]
concatRight list = foldr (either (const . Left) (fmap . (:))) (Right []) list
