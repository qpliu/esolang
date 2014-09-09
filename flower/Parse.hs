module Parse
    (Statement(SendAllStatement,IfStatement,SendStatement,BindStatement),
     IfBlock(IfBlock),
     Expression(ExpressionLiteral,ExpressionBound,ExpressionCall),
     Symbol(Symbol),
     Literal(Literal),
     parser,resolver,parserResolver)
where

import Data.Either(partitionEithers)
import Text.ParserCombinators.Parsec
    (Parser,ParseError,SourcePos,
     getPosition,many,many1,manyTill,
     newline,noneOf,parse,space,string,try,
     (<|>))

data Definition = Definition SourcePos Symbol [Symbol] [Symbol] [Statement]
    deriving Show

data Statement = SendAllStatement SourcePos
               | IfStatement SourcePos [IfBlock]
               | SendStatement SourcePos [Expression] [Symbol]
               | BindStatement SourcePos [Symbol] [Expression]
    deriving Show

data IfBlock = IfBlock [Expression] [Statement]
    deriving Show

data Expression = ExpressionLiteral Literal
                | ExpressionBound Symbol
                | ExpressionCall Symbol [Expression] Int
                | ExpressionUndetermined [Either Symbol Literal]
    deriving Show

data Symbol = Symbol SourcePos String
    deriving Show

data Literal = Literal SourcePos Bool
    deriving Show

parser :: String -> String -> Either ParseError [Definition]
parser filename source =
    parse (skipSpace False >> many definition) filename source

skipSpace :: Bool -> Parser ()
skipSpace required = do
    if required
        then (space >> return ()) <|> comment
        else return ()
    many ((space >> return ()) <|> comment)
    return ()

comment :: Parser ()
comment = do
    try (string "--")
    manyTill (noneOf "\n") newline
    return ()

definition :: Parser Definition
definition = do
    position <- getPosition
    name <- symbol
    inputs <- many symbol
    token "->" True
    outputs <- many symbol
    statements <- statementBlock
    return (Definition position name inputs outputs statements)

symbol :: Parser Symbol
symbol = try $ do
    position <- getPosition
    str <- fmap reverse (symbolString "")
    if str `elem` ["", "0", "1", "<-", "->", "if", "else"]
        then fail ("Invalid symbol: " ++ str)
        else return (Symbol position str)
  where
    symbolString str =
        (try (skipSpace True) >> return str)
            <|> (noneOf "{} \t\f\r\n;" >>= symbolString . (:str))
            <|> return str

token :: String -> Bool -> Parser ()
token tok checkSpace = do
    string tok
    skipSpace checkSpace
    return ()

statementBlock :: Parser [Statement]
statementBlock = do
    token "{" False
    statements <- many statement
    token "}" False
    return statements

statement :: Parser Statement
statement =
    try sendAllStatement <|> try ifStatement <|> try bindStatement <|> sendStatement

sendAllStatement :: Parser Statement
sendAllStatement = do
    position <- getPosition
    token "->" True
    token ";" False
    return (SendAllStatement position)

ifStatement :: Parser Statement
ifStatement = do
    position <- getPosition
    ifBlocks <- many1 ifBlock
    elseBlock <- statementBlock
    return (IfStatement position (ifBlocks ++ [IfBlock [] elseBlock]))

ifBlock :: Parser IfBlock
ifBlock = do
    token "if" True
    expr <- undeterminedExpression
    statements <- statementBlock
    token "else" True
    return (IfBlock [expr] statements)

bindStatement :: Parser Statement
bindStatement = do
    position <- getPosition
    symbols <- many1 symbol
    token "<-" True
    expression <- undeterminedExpression
    token ";" False
    return (BindStatement position symbols [expression])

sendStatement :: Parser Statement
sendStatement = do
    position <- getPosition
    expression <- undeterminedExpression
    token "->" True
    symbols <- many1 symbol
    token ";" False
    return (SendStatement position [expression] symbols)

undeterminedExpression :: Parser Expression
undeterminedExpression = do
    exprs <- many1 undeterminedExprToken
    return (ExpressionUndetermined exprs)
  where
    undeterminedExprToken = do
        position <- getPosition
        (try (token "0" True) >> return (Right (Literal position False)))
            <|> (try (token "1" True) >> return (Right (Literal position True)))
            <|> fmap Left symbol

parserResolver :: String -> String -> Either [ParseError] [Definition]
parserResolver filename source =
    either (Left . (:[])) resolver (parser filename source)

resolver :: [Definition] -> Either [ParseError] [Definition]
resolver definitions = do
    arity <- arities definitions
    case partitionEithers (map (resolveAndCheck arity) definitions) of
        ([],definitions) -> Right definitions
        (errors,_) -> Left errors
  where
    resolveAndCheck arity definition =
        resolveUndetermined arity definition >>= checkDuplicateParameters
            >>= checkIfBlocks >>= checkBind >>= checkSend >>= resolveSendAll
            >>= checkOutputs

-- check for duplicate function names
arities :: [Definition] -> Either [ParseError] (String -> Maybe (Int,Int))
arities definitions = undefined

resolveUndetermined :: (String -> Maybe (Int,Int)) -> Definition -> Either ParseError Definition
resolveUndetermined arity definition = undefined

checkDuplicateParameters :: Definition -> Either ParseError Definition
checkDuplicateParameters = undefined

-- make sure there is either 1 or a non-zero even number of condition
-- values in each branch except the last, which should have 0
checkIfBlocks :: Definition -> Either ParseError Definition
checkIfBlocks = undefined

-- make sure no duplicate symbols are bound and make sure
-- number of symbols being bound match the number of values
checkBind :: Definition -> Either ParseError Definition
checkBind = undefined

-- make sure the number of output symbols match the number of values
checkSend :: Definition -> Either ParseError Definition
checkSend = undefined

resolveSendAll :: Definition -> Either ParseError Definition
resolveSendAll = undefined

-- make sure each branch sends to the same outputs
checkIfOutputs :: Definition -> Either ParseError Definition
checkIfOutputs = undefined

-- make sure each output is sent to exactly once
checkOutputs :: Definition -> Either ParseError Definition
checkOutputs = undefined
