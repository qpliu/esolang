module Parser1
    (Defn1(Defn1),
     Expr1(Expr1Literal,Expr1Symbol),
     Param1(Param1Bound,Param1Literal,Param1Wild),
     parser1)
where

import Text.ParserCombinators.Parsec
    (Parser,ParseError,SourcePos,
     char,getPosition,many,many1,manyTill,
     newline,noneOf,oneOf,optionMaybe,optional,parse,space,string,try,
     (<|>))

-- Parser pass 1.

data Defn1 = Defn1 SourcePos String [Param1] [Expr1]
    deriving Show

data Expr1 = Expr1Literal SourcePos [Bool]
           | Expr1Symbol SourcePos String
    deriving Show

data Param1 = Param1Bound SourcePos [Bool] String
            | Param1Literal SourcePos [Bool]
            | Param1Wild SourcePos [Bool]
    deriving Show

parser1 :: String -> String -> Either ParseError [Defn1]
parser1 filename source =
    parse (skipSpace >> many defn) filename source

defn :: Parser Defn1
defn = do
    position <- getPosition
    name <- symbol
    params <- many (try param)
    lastParam <- fmap (maybe [] (:[])) (optionMaybe lastParam)
    char '='
    skipSpace
    exprs <- many expr
    char '.'
    skipSpace
    return (Defn1 position name (params ++ lastParam) exprs)

symbol :: Parser String
symbol = do
    name <- many1 (noneOf " \t\r\n=._01")
    skipSpace
    return name

param :: Parser Param1
param = do
    position <- getPosition
    bits <- many bit
    param <- fmap (Param1Bound position bits) symbol
                <|> (char '.' >> return (Param1Wild position bits))
                <|> (char '_' >> return (Param1Literal position bits))
    skipSpace
    return param

lastParam :: Parser Param1
lastParam = do
    position <- getPosition
    fmap (Param1Literal position) (many1 bit)

expr :: Parser Expr1
expr = do
    position <- getPosition
    fmap (Expr1Symbol position) symbol <|> fmap (Expr1Literal position) literal

skipSpace :: Parser ()
skipSpace = many ((space >> return ()) <|> comment) >> return ()

comment :: Parser ()
comment = do
    try (string "==")
    manyTill (noneOf "\n") newline
    return ()

bit :: Parser Bool
bit = do
    c <- oneOf "01"
    skipSpace
    return (c == '1')

literal :: Parser [Bool]
literal = nilLiteral <|> nonNilLiteral

nilLiteral :: Parser [Bool]
nilLiteral = do
    char '_'
    skipSpace
    return []

nonNilLiteral :: Parser [Bool]
nonNilLiteral = do
    bits <- many1 bit
    optional (char '_')
    skipSpace
    return bits
