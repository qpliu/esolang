module Parse
    (parse)
where

import Control.Monad(void)

import Text.Parsec.Error(errorMessages,errorPos,messageString)
import Text.ParserCombinators.Parsec
    (ParseError(..),Parser,SourcePos,
     anyChar,char,eof,getPosition,many,many1,manyTill,
     oneOf,newline,noneOf,skipMany,string,try,
     (<|>))
import qualified Text.ParserCombinators.Parsec as Parsec

import Ast(Identifier(..),PartialDef(..),Token(..))
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
        try (string "--")
        manyTill anyChar newline
        return ()

identifier :: Parser Identifier
identifier = do
    pos <- getPosition
    ident <- many1 (noneOf " \f\r\n\t=.[]()*|&^∪∩△⊖×")
    skipSpace
    return (Identifier pos ident)

equals :: Parser ()
equals = do
    char '='
    skipSpace

period :: Parser ()
period = do
    char '.'
    skipSpace

token :: Parser Token
token = do
    pos <- getPosition
    tok <- try (string "[")
        <|> try (string "]")
        <|> try (string "(")
        <|> try (string ")")
        <|> try (string "*")
        <|> try (string "|")
        <|> try (string "&")
        <|> try (string "^")
        <|> try (string "∪")
        <|> try (string "∩")
        <|> try (string "△")
        <|> try (string "⊖")
        <|> try (string "×")
        <|> many1 (noneOf " \f\r\n\t=.[]()*|&^∪∩△⊖×")
    skipSpace
    return (Token pos tok)

partialDef :: Parser PartialDef
partialDef = do
    name <- identifier
    params <- many identifier
    equals
    body <- many token
    period
    return (PartialDef name params body)
