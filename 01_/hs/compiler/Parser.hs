module Parser
    (Defn(Defn),
     Expr(ExprLiteral,ExprParam,ExprFuncall),
     Param(ParamBound,ParamLiteral,ParamWild),
     arity,parse,parseFiles)
where

import Data.Map(Map)
import qualified Data.Map
import Text.ParserCombinators.Parsec(ParseError)

import Parser1(parser1)
import Parser2(parser2)
import Parser3
    (Defn(Defn),
     Expr(ExprLiteral,ExprParam,ExprFuncall),
     Param(ParamBound,ParamLiteral,ParamWild),
     parser3)
import Parser1(Defn1)

parse :: [(FilePath,String)] -> Either ParseError (Map String [Defn])
parse sourceFiles =
    apply parser3 $ apply parser2
                  $ concatRight $ map (uncurry parser1) sourceFiles

parseFiles :: [FilePath] -> IO (Either ParseError (Map String [Defn]))
parseFiles filenames =
    fmap (parse . zip filenames) (sequence (map readFile filenames))

parse1 :: [(FilePath,String)] -> Either ParseError [Defn1]
parse1 sourceFiles = concatRight $ map (uncurry parser1) sourceFiles

parse1Files :: [FilePath] -> IO (Either ParseError [Defn1])
parse1Files filenames =
    fmap (parse1 . zip filenames) (sequence (map readFile filenames))

parse2 :: [(FilePath,String)] -> Either ParseError (Map String [Defn1])
parse2 sourceFiles =
    apply parser2 $ concatRight $ map (uncurry parser1) sourceFiles

parse2Files :: [FilePath] -> IO (Either ParseError (Map String [Defn1]))
parse2Files filenames =
    fmap (parse2 . zip filenames) (sequence (map readFile filenames))

apply :: (a -> Either b c) -> Either b a -> Either b c
apply f e = either Left f e

concatRight :: [Either a [b]] -> Either a [b]
concatRight list = foldl (either (const . Left) (fmap . (++))) (Right []) list

arity :: Map String [Defn] -> String -> Maybe Int
arity fns name = fmap getArity (Data.Map.lookup name fns)
  where
    getArity (Defn params _:_) = length params
