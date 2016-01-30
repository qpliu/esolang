module Parse
    (parse)
where

import Control.Monad(unless,void,when)

import Text.Parsec.Error(errorMessages,errorPos,messageString)
import Text.ParserCombinators.Parsec
    (ParseError(..),Parser,SourcePos,
     anyChar,char,eof,getPosition,lookAhead,many,many1,manyTill,
     oneOf,optional,newline,noneOf,skipMany,string,try,
     (<|>))
import qualified Text.ParserCombinators.Parsec as Parsec

import Ast(Identifier(..),PartialDef(..),Param(..),Unparsed(..))
import Compile(Compile(..),CompileError(..),SourcePos)

parse :: String -> String -> Compile [PartialDef]
parse filename source =
    Compile (either (Left . mapError) Right
                    (Parsec.parse parser filename source))
  where
    parser = do
        skipSpace
        defs <- many partialDef
        eof
        return defs
    mapError err =
        CompileError (errorPos err) (messageString (last (errorMessages err)))

skipSpace :: Parser ()
skipSpace = skipMany (void (oneOf " \f\r\n\t") <|> skipComment)
  where
    skipComment = do
        try (string "==")
        manyTill anyChar newline
        return ()

identifier :: Parser Identifier
identifier = do
    pos <- getPosition
    ident <- many1 (noneOf " \f\r\n\t01_=.")
    skipSpace
    return (Identifier pos ident)

token :: Char -> Parser ()
token c = do
    char c
    skipSpace

literal :: Parser [Bool]
literal = literalNil <|> literalBits
  where
    literalNil = token '_' >> return []
    literalBits = do
        bits <- many1 bit
        optional (token '_')
        return bits

bit :: Parser Bool
bit = (token '0' >> return False) <|> (token '1' >> return True)

partialDef :: Parser PartialDef
partialDef = do
    name <- identifier
    params <- many param
    token '='
    body <- many unparsed
    token '.'
    return (PartialDef name params body)

param :: Parser Param
param = do
    pos <- getPosition
    bits <- many bit
    (lookAhead (char '=') >> return (ParamIgnored pos bits))
        <|> (token '.' >> return (ParamIgnored pos bits))
        <|> (token '_' >> return (ParamLiteral pos bits))
        <|> (identifier >>= (return . ParamBound pos bits))

unparsed :: Parser Unparsed
unparsed = do
    pos <- getPosition
    fmap UnparsedIdentifier identifier <|> fmap (UnparsedLiteral pos) literal
