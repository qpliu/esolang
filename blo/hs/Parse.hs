module Parse
    (ParseError,SourcePos,
     Identifier(..),Definition(..),
     FuncHeader(..),TypeField(..),Var(..),
     Stmt(..),Expr(..),
     parse)
where

import Control.Monad(unless,void,when)
import Data.Set(Set,fromList,member)
import Text.ParserCombinators.Parsec
    (ParseError,Parser,SourcePos,
     anyChar,char,eof,getPosition,lookAhead,many,many1,manyTill,
     oneOf,optionMaybe,newline,noneOf,
     sepBy,skipMany,string,try,
     (<|>),(<?>))
import qualified Text.ParserCombinators.Parsec as Parsec

parse :: String -> String -> Either ParseError [Definition]
parse filename source = Parsec.parse parser filename source
  where
    parser = do
        skipNewlines
        defs <- many definition
        skipNewlines
        many (oneOf spaceChars)
        eof
        return defs

keywords :: [String]
keywords = ["type", "func", "var", "if", "else", "for", "break",
            "return", "set", "clear", "import"]

tokenChars :: String
tokenChars = "={}().,;\n"

spaceChars :: String
spaceChars = " \t\v\r" -- does not include newline

nextToken :: Parser (SourcePos,String)
nextToken = do
    many (void (oneOf spaceChars) <|> skipLineComment <|> skipBlockComment)
    pos <- getPosition
    tok <- singleCharToken <|> continueToken ""
    return (pos,tok)
  where
    singleCharToken = fmap (:[]) (oneOf tokenChars)
    continueToken pre = do
        rest <- (if pre == "" then many1 else many) (noneOf ('/' : tokenChars ++ spaceChars))
        try (throughSlash (pre ++ rest)) <|> return (pre ++ rest)
    throughSlash pre = do
        try (lookAhead (string "/*")) <|> try (lookAhead (string "//"))
                                      <|> (char '/' >> continueToken (pre ++ "/"))
    skipLineComment = do
        try (string "//")
        manyTill (noneOf "\n") newline
        return ()
    skipBlockComment = do
        try (string "/*")
        manyTill anyChar (try (string "*/"))
        return ()

skipNewlines :: Parser ()
skipNewlines = void (many (try (token "\n")))

terminateStmt :: Parser ()
terminateStmt = void (try (token ";") <|> try (token "\n")
                                      <|> try (lookAhead (token "{"))
                                      <|> try (lookAhead (token "}")))

token :: String -> Parser SourcePos
token tok = do
    (pos,t) <- nextToken
    unless (t == tok) (fail ("Expected '" ++ tok ++ "'"))
    return pos

data Identifier = Identifier SourcePos String
  deriving Show

data Definition =
    FuncDef SourcePos FuncHeader Stmt
  | FuncImport SourcePos FuncHeader
  | TypeDef SourcePos Identifier [TypeField]
  | TypeImport SourcePos Identifier [TypeField]
  deriving Show

data FuncHeader = FuncHeader Identifier [Var] (Maybe Identifier)
  deriving Show
data TypeField = TypeField Identifier (Maybe Identifier)
  deriving Show
data Var = Var Identifier Identifier
  deriving Show
data Stmt =
    StmtBlock SourcePos [Stmt]
  | StmtVar SourcePos Var (Maybe Expr)
  | StmtIf SourcePos Expr Stmt (Maybe Stmt)
  | StmtFor SourcePos (Maybe Identifier) Stmt
  | StmtBreak SourcePos (Maybe Identifier)
  | StmtReturn SourcePos (Maybe Expr)
  | StmtSetClear SourcePos Bool Expr
  | StmtAssign SourcePos Expr Expr
  | StmtExpr Expr
  deriving Show
data Expr =
    ExprVar Identifier
  | ExprFunc Identifier [Expr]
  | ExprField Expr Identifier
  deriving Show

identifier :: Parser Identifier
identifier = do
    (pos,tok) <- nextToken
    if member tok nonIdentifiers
        then fail "Expected identifier"
        else return (Identifier pos tok)
  where
    nonIdentifiers = fromList (keywords ++ map (:[]) tokenChars)

definition :: Parser Definition
definition = do
    funcDef <|> typeDef <|> importDef <?> "'func', 'type', or 'import'"
  where
    funcDef = do
        pos <- try (token "func")
        header <- funcHeader
        skipNewlines
        body <- stmtBlock
        skipNewlines
        return (FuncDef pos header body)
    typeDef = do
        pos <- try (token "type")
        skipNewlines
        name <- identifier <?> "type name"
        skipNewlines
        token "{"
        skipNewlines
        fields <- typeFields [] [] <|> return []
        token "}"
        skipNewlines
        return (TypeDef pos name fields)
    importDef = do
        pos <- try (token "import")
        skipNewlines
        funcImport pos <|> typeImport pos <?> "'func', 'type'"
    funcImport pos = do
        try (token "func")
        header <- funcHeader
        skipNewlines
        return (FuncImport pos header)
    typeImport pos = do
        try (token "type")
        skipNewlines
        name <- identifier <?> "type name"
        skipNewlines
        token "{"
        skipNewlines
        fields <- typeFields [] [] <|> return []
        token "}"
        skipNewlines
        return (TypeImport pos name fields)

funcHeader :: Parser FuncHeader
funcHeader = do
    skipNewlines
    name <- identifier <?> "func name"
    skipNewlines
    token "("
    skipNewlines
    params <- funcParams [] [] <|> return []
    token ")"
    skipNewlines
    returnType <- optionMaybe (try identifier)
    return (FuncHeader name params returnType)

funcParams :: [Var] -> [Identifier] -> Parser [Var]
funcParams pendingVars pendingVarNames = do
    varName <- try identifier <?> "parameter name"
    skipNewlines
    anotherVarWithSameType varName <|> addVarType varName
  where
    anotherVarWithSameType varName = do
        try (token ",")
        skipNewlines
        funcParams pendingVars (varName:pendingVarNames)
    addVarType varName = do
        varType <- identifier <?> "parameter type"
        skipNewlines
        let vars = map (flip Var varType) (varName:pendingVarNames) ++ pendingVars
        anotherParam vars <|> return (reverse vars)
    anotherParam vars = do
        try (token ",")
        skipNewlines
        funcParams vars []

typeFields :: [TypeField] -> [Identifier] -> Parser [TypeField]
typeFields pendingFields pendingFieldNames = do
    fieldName <- try identifier <?> "field name"
    anotherFieldWithSameType fieldName <|> addFieldType fieldName
  where
    anotherFieldWithSameType fieldName = do
        try (token ",")
        skipNewlines
        typeFields pendingFields (fieldName:pendingFieldNames)
    addFieldType fieldName = do
        fieldType <- optionMaybe (try identifier)
        terminateField
        skipNewlines
        let fields = ((map (flip TypeField fieldType) (fieldName:pendingFieldNames)) ++ pendingFields)
        typeFields fields [] <|> return (reverse fields)
    terminateField = void (try (token ";") <|> try (token "\n")
                                           <|> try (lookAhead (token "}")))

stmt :: Parser Stmt
stmt = do
    stmtBlock <|> stmtVar <|> stmtIf <|> stmtFor <|> stmtBreak
              <|> stmtReturn <|> stmtSet <|> stmtClear <|> stmtAssignOrExpr

stmtBlock :: Parser Stmt
stmtBlock = do
    pos <- try (token "{")
    skipNewlines
    stmts <- many stmt
    skipNewlines
    token "}"
    return (StmtBlock pos stmts)

stmtVar :: Parser Stmt
stmtVar = do
    pos <- try (token "var")
    varName <- identifier <?> "variable name"
    varType <- identifier <?> "variable type"
    init <- optionMaybe initializer
    terminateStmt
    return (StmtVar pos (Var varName varType) init)
  where
    initializer = do
        try (token "=")
        skipNewlines
        expr False

stmtIf :: Parser Stmt
stmtIf = do
    pos <- try (token "if")
    ex <- expr True
    stmts <- stmtBlock
    skipNewlines
    elseStmt <- optionMaybe stmtElse
    return (StmtIf pos ex stmts elseStmt)
  where
    stmtElse = do
        try (token "else")
        skipNewlines
        stmtIf <|> stmtBlock

stmtFor :: Parser Stmt
stmtFor = do
    pos <- try (token "for")
    skipNewlines
    label <- optionMaybe (try identifier)
    skipNewlines
    stmts <- stmtBlock
    return (StmtFor pos label stmts)

stmtBreak :: Parser Stmt
stmtBreak = do
    pos <- try (token "break")
    label <- optionMaybe (try identifier)
    terminateStmt
    return (StmtBreak pos label)

stmtReturn :: Parser Stmt
stmtReturn = do
    pos <- try (token "return")
    ex <- optionMaybe (expr False)
    terminateStmt
    return (StmtReturn pos ex)

stmtSet :: Parser Stmt
stmtSet = do
    pos <- try (token "set")
    skipNewlines
    ex <- expr False
    terminateStmt
    return (StmtSetClear pos True ex)

stmtClear :: Parser Stmt
stmtClear = do
    pos <- try (token "clear")
    skipNewlines
    ex <- expr False
    terminateStmt
    return (StmtSetClear pos False ex)

stmtAssignOrExpr :: Parser Stmt
stmtAssignOrExpr = do
    ex <- expr False
    stmtAssign ex <|> stmtExpr ex
  where
    stmtAssign lhs = do
        pos <- try (token "=")
        skipNewlines
        rhs <- expr False
        terminateStmt
        return (StmtAssign pos lhs rhs)
    stmtExpr ex = do
        terminateStmt
        return (StmtExpr ex)

expr :: Bool -> Parser Expr
expr ignoreNewlines = do
    when ignoreNewlines skipNewlines
    name <- try identifier <?> "variable or func name"
    when ignoreNewlines skipNewlines
    exprField (ExprVar name) <|> exprFunc name <|> return (ExprVar name)
  where
    exprField ex = do
        try (token ".")
        skipNewlines
        name <- identifier <?> "field name"
        when ignoreNewlines skipNewlines
        exprField (ExprField ex name) <|> return (ExprField ex name)
    exprFunc name = do
        try (token "(")
        args <- sepBy (expr True) (try (token ","))
        skipNewlines
        token ")"
        when ignoreNewlines skipNewlines
        exprField (ExprFunc name args) <|> return (ExprFunc name args)
