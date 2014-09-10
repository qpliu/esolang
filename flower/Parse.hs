module Parse
    (Statement(IfStatement,SendStatement,BindStatement),
     IfBlock(IfBlock),
     Expression(ExpressionLiteral,ExpressionBound,ExpressionCall),
     Symbol(Symbol),
     Literal(Literal),
     parse,resolve,parseResolve,prettyPrint)
where

import Data.Either(partitionEithers)
import Data.List(intercalate,nub,partition,sort)
import Data.Map(Map,empty,insert,member)
import qualified Data.Map
import Text.ParserCombinators.Parsec
    (Parser,ParseError,SourcePos,
     getPosition,lookAhead,many,many1,manyTill,
     newline,noneOf,oneOf,space,string,try,
     (<|>))
import qualified Text.ParserCombinators.Parsec
import Text.Parsec.Error(Message(Message),newErrorMessage)

data Definition = Definition SourcePos Symbol [Symbol] [Symbol] [Statement]
    deriving Show

data Statement = SendAllStatement SourcePos
               | IfStatement SourcePos [IfBlock]
               | SendStatement SourcePos Bool [Expression] [Symbol] -- Bool is True for resolved SendAll, False otherwise
               | BindStatement SourcePos [Symbol] [Expression]
    deriving Show

data IfBlock = IfBlock SourcePos [Expression] [Statement]
    deriving Show

data Expression = ExpressionLiteral Literal
                | ExpressionBound Symbol
                | ExpressionCall Symbol [Expression] Int -- Int is number of outputs
                | ExpressionUndetermined [Either Symbol Literal]
    deriving Show

data Symbol = Symbol SourcePos String
    deriving Show

instance Eq Symbol where
    (Symbol _ name1) == (Symbol _ name2) = name1 == name2

instance Ord Symbol where
    compare (Symbol _ name1) (Symbol _ name2) = compare name1 name2

data Literal = Literal SourcePos Bool
    deriving Show

parse :: String -> String -> Either ParseError [Definition]
parse filename source = Text.ParserCombinators.Parsec.parse (skipSpace False >> many definition) filename source

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
    skipSpace checkSpace <|> (lookAhead (oneOf "{};") >> return ())
    return ()

statementBlock :: Parser [Statement]
statementBlock = do 
    token "{" False
    statements <- many statement
    token "}" False
    return statements

statement :: Parser Statement
statement = try sendAllStatement
        <|> try ifStatement
        <|> try bindStatement
        <|> sendStatement

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
    elsePosition <- getPosition
    elseBlock <- statementBlock
    return (IfStatement position (ifBlocks ++ [IfBlock elsePosition [] elseBlock]))

ifBlock :: Parser IfBlock
ifBlock = do
    position <- getPosition
    token "if" True
    expr <- undeterminedExpression
    statements <- statementBlock
    token "else" True
    return (IfBlock position [expr] statements)

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
    return (SendStatement position False [expression] symbols)

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

parseResolve :: String -> String -> Either [ParseError] [Definition]
parseResolve filename source =
    either (Left . (:[])) resolve (parse filename source)

resolve :: [Definition] -> Either [ParseError] [Definition]
resolve definitions = do
    arity <- arities definitions
    case partitionEithers (map (resolveAndCheck arity) definitions) of
        ([],definitions) -> Right definitions
        (errors,_) -> Left (concat errors)
  where
    resolveAndCheck arity definition =
        resolveUndetermined arity definition
            >>= check checkDuplicateParameters
            >>= check checkIfBlocks >>= check checkBind >>= check checkSend
            >>= resolveSendAll

symbolName :: Symbol -> String
symbolName (Symbol _ name) = name

outputCount :: Expression -> Int
outputCount (ExpressionLiteral _) = 1
outputCount (ExpressionBound _) = 1
outputCount (ExpressionCall _ _ count) = count
outputCount expression = error ("outputCount:" ++ show expression)

expressionPosition :: Expression -> SourcePos
expressionPosition (ExpressionLiteral (Literal position _)) = position
expressionPosition (ExpressionBound (Symbol position _)) = position
expressionPosition (ExpressionCall (Symbol position _) _ _) = position
expressionPosition expression = error ("expressionPosition:" ++ show expression)

-- check for duplicate function names
arities :: [Definition] -> Either [ParseError] (String -> Maybe (Int,Int))
arities definitions =
    case foldl collect ([],empty) definitions of
        ([],arityMap) -> Right (flip Data.Map.lookup arityMap)
        (errors,_) -> Left (reverse errors)
  where
    collect (errors,arityMap)
            (Definition position (Symbol _ name) inputs outputs _)
      | name `member` arityMap = (newErrorMessage (Message ("Already defined: " ++ name)) position:errors,arityMap)
      | otherwise = (errors,insert name (length inputs,length outputs) arityMap)

resolveUndetermined :: (String -> Maybe (Int,Int)) -> Definition -> Either [ParseError] Definition
resolveUndetermined arity (Definition position name inputs outputs statements) =
    case foldl resolve ([],map symbolName inputs,[]) statements of
        ([],_,statements) -> Right (Definition position name inputs outputs (reverse statements))
        (errors,_,_) -> Left (reverse errors)
  where
    resolve (errors,locals,statements) statement =
        case statement of
            (SendAllStatement _) -> (errors,locals,statement:statements)
            (IfStatement position ifBlocks) ->
                case partitionEithers (map (resolveIfBlock locals) ifBlocks) of
                    ([],ifBlocks) -> (errors,locals,IfStatement position ifBlocks:statements)
                    (newErrors,_) -> (concat newErrors ++ errors,locals,[])
            (SendStatement position False expressions symbols) ->
                case resolveUndeterminedExpressions arity locals expressions of
                    Left newErrors -> (newErrors ++ errors,locals,[])
                    Right expressions -> (errors,locals,SendStatement position False expressions symbols:statements)
            (BindStatement position symbols expressions) ->
                case resolveUndeterminedExpressions arity locals expressions of
                    Left newErrors -> (newErrors ++ errors,locals,[])
                    Right expressions -> (errors,map symbolName symbols ++ locals,BindStatement position symbols expressions:statements)
    resolveIfBlock locals (IfBlock position expressions body) =
        case (resolveUndeterminedExpressions arity locals expressions,
              foldl resolve ([],locals,[]) body) of
            (Right expressions,([],_,body)) -> Right (IfBlock position expressions (reverse body))
            (Left errors,(bodyErrors,_,_)) -> Left (errors ++ bodyErrors)
            (_,(bodyErrors,_,_)) -> Left bodyErrors
    
check :: (Definition -> [ParseError]) -> Definition -> Either [ParseError] Definition
check checker definition =
    case checker definition of
        [] -> Right definition
        errors -> Left errors

checkDuplicateParameters :: Definition -> [ParseError]
checkDuplicateParameters (Definition _ _ inputs outputs _) =
    case (foldl (collect "input") ([],[]) inputs,foldl (collect "output") ([],[]) outputs) of
        ((inputErrors,_),(outputErrors,_)) -> reverse inputErrors ++ reverse outputErrors
  where
    collect parameterType (errors,names) (Symbol position name)
      | name `elem` names = (newErrorMessage (Message ("Duplicate " ++ parameterType ++ " parameter: " ++ name)) position:errors,names)
      | otherwise = (errors,name:names)

-- make sure there is either 1 or a non-zero even number of condition
-- values in each branch except the last, which should have 0
checkIfBlocks :: Definition -> [ParseError]
checkIfBlocks (Definition _ _ _ _ body) = reverse (foldl checkIf [] body)
  where
    checkIf errors (IfStatement _ ifBlocks) = checkBlock errors ifBlocks
    checkIf errors _ = errors
    checkBlock errors [] = error ("checkIfBlocks:" ++ show body)
    checkBlock errors (IfBlock _ [] body:[]) = foldl checkIf errors body
    checkBlock errors (_:[]) = error ("checkIfBlocks:" ++ show body)
    checkBlock errors (IfBlock position expressions body:ifBlocks) =
        let count = sum (map outputCount expressions)
        in  if count == 1 || (count `mod` 2 == 0 && count > 1) then
                checkBlock (foldl checkIf errors body) ifBlocks
            else
                checkBlock (newErrorMessage (Message ("Invalid if condition count:" ++ show count)) position:errors) ifBlocks

-- make sure no duplicate symbols are bound and make sure
-- number of symbols being bound match the number of values
checkBind :: Definition -> [ParseError]
checkBind (Definition _ _ _ _ body) = reverse (foldl checkB [] body)
  where
    checkB errors (IfStatement _ ifBlocks) = foldl checkIfBlock errors ifBlocks
    checkB errors (BindStatement position symbols expressions)
      | length symbols == sum (map outputCount expressions) = errors
      | otherwise = newErrorMessage (Message "Number of symbols bound does not match number of values") position:errors
    checkB errors _ = errors
    checkIfBlock errors (IfBlock _ _ body) = foldl checkB errors body

-- make sure the number of output symbols match the number of values
checkSend :: Definition -> [ParseError]
checkSend (Definition _ _ _ _ body) = reverse (foldl checkS [] body)
  where
    checkS errors (IfStatement _ ifBlocks) = foldl checkIfBlock errors ifBlocks
    checkS errors (SendStatement position _ expressions symbols)
      | length symbols == sum (map outputCount expressions) = errors
      | otherwise = newErrorMessage (Message "Number of outputs does not match number of values") position:errors
    checkS errors _ = errors
    checkIfBlock errors (IfBlock _ _ body) = foldl checkS errors body

-- make sure each branch sends to the same outputs
-- make sure each output is sent to exactly once
resolveSendAll :: Definition -> Either [ParseError] Definition
resolveSendAll (Definition position name inputs outputs statements) = do
    (unsent,resolved) <- resolveSends inputs outputs statements []
    if null unsent
        then return (Definition position name inputs outputs resolved)
        else Left [newErrorMessage (Message ("Unused output(s):" ++ show (map symbolName unsent))) position]
  where
    resolveSends locals outputs statements resolved =
        case statements of
            [] -> Right (outputs,reverse resolved)
            (SendAllStatement position:statements) ->
                let (sent,unsent) = partition (`elem` locals) outputs
                in  resolveSends locals unsent statements (SendStatement position True (map ExpressionBound sent) sent:resolved)
            (IfStatement position ifBlocks:statements) ->
                resolveIfBlocks locals outputs statements resolved position ifBlocks
            (statement@(SendStatement position _ _ symbols):statements) ->
                case (filter (`notElem` outputs) symbols,filter (`notElem` symbols) outputs) of
                    ([],unsent) -> resolveSends locals unsent statements (statement:resolved)
                    (invalid,_) -> Left [newErrorMessage (Message ("Invalid output(s):" ++ show (map symbolName invalid))) position]
            (statement@(BindStatement _ symbols _):statements) ->
                resolveSends (symbols ++ locals) outputs statements (statement:resolved)
    ifBlockStatements (IfBlock _ _ statements) = statements
    resolveIfBlocks locals outputs statements resolved ifPosition ifBlocks =
        case partitionEithers (map (flip (resolveSends locals outputs) [] . ifBlockStatements) ifBlocks) of
            ([],resolvedBlocks) -> checkBlocks resolvedBlocks
            (errors,_) -> Left (concat errors)
      where
        checkBlocks blockResolutions
          | length (nub (map (sort . fst) blockResolutions)) /= 1 =
                Left [newErrorMessage (Message "Unmatching outputs in if-branches") ifPosition]
          | otherwise =
                resolveSends locals (fst (head blockResolutions)) statements (IfStatement ifPosition (zipWith replaceBody blockResolutions ifBlocks):resolved)
    replaceBody (_,statements) (IfBlock position expressions _) = IfBlock position expressions statements

-- resolve undetermined expressions
resolveUndeterminedExpressions :: (String -> Maybe (Int,Int)) -> [String] -> [Expression] -> Either [ParseError] [Expression]
resolveUndeterminedExpressions arity locals expressions =
    case expressions of
        [] -> Right []
        [ExpressionUndetermined tokens] -> resolve [] (reverse tokens)
        _ -> error ("resolveUndeterminedExpressions:" ++ show expressions)
  where
    resolve stack [] = Right stack
    resolve stack (Right literal:tokens) =
        resolve (ExpressionLiteral literal:stack) tokens
    resolve stack (Left symbol@(Symbol position name):tokens)
      | name `elem` locals = resolve (ExpressionBound symbol:stack) tokens
      | otherwise =
            case arity name of
                Nothing -> Left [newErrorMessage (Message ("Undefined symbol: " ++ name)) position]
                Just (inputs,outputs) ->
                    case takeArgs position inputs stack 0 [] of
                        Left error -> Left [error]
                        Right (args,stack) -> resolve (ExpressionCall symbol (reverse args) outputs:stack) tokens
    takeArgs position inputs stack argc args
      | argc == inputs = Right (args,stack)
      | argc > inputs = Left (newErrorMessage (Message ("Argument overflow: " ++ show inputs ++ " input(s) required, got " ++ show argc ++ " inputs")) position)
      | otherwise =
            case stack of
                [] -> Left (newErrorMessage (Message ("Not enough arguments: " ++ show inputs ++ " input(s) required, got " ++ show argc ++ " inputs")) position)
                expr@(ExpressionCall _ _  outputs):stack ->
                    takeArgs position inputs stack (argc+outputs) (expr:args)
                expr:stack ->
                    takeArgs position inputs stack (argc+1) (expr:args)

prettyPrint :: Definition -> String
prettyPrint (Definition _ name inputs outputs statements) =
    symbolName name ++ " "
        ++ symbolNames inputs ++ " -> " ++ symbolNames outputs
        ++ " {\n" ++ concatMap (ppStatement 1) statements ++ "}\n"
  where
    indent indentLevel = replicate (4 * indentLevel) ' '
    symbolNames = intercalate " " . map symbolName
    ppExpressions = intercalate " " . map ppExpression
    ppStatement indentLevel statement =
        case statement of
            IfStatement _ ifBlocks ->
                indent indentLevel ++ concatMap (ppIfBlock indentLevel) ifBlocks
            SendStatement _ True _ _ -> indent indentLevel ++ "->;\n"
            SendStatement _ False expressions symbols ->
                indent indentLevel ++ ppExpressions expressions
                    ++ " -> " ++ symbolNames symbols ++ ";\n"
            BindStatement _ symbols expressions ->
                indent indentLevel ++ symbolNames symbols
                    ++ " <- " ++ ppExpressions expressions ++ ";\n"
            _ -> error ("prettyPrint:" ++ show statement)
    ppIfBlock indentLevel (IfBlock _ expressions statements)
      | null expressions = "{\n" ++ concatMap (ppStatement (indentLevel + 1)) statements ++ indent indentLevel ++ "}\n"
      | otherwise = "if " ++ ppExpressions expressions ++ " {\n" ++ concatMap (ppStatement (indentLevel + 1)) statements ++ indent indentLevel ++ "} else "
    ppExpression expression =
        case expression of
            ExpressionLiteral (Literal _ True) -> "1"
            ExpressionLiteral (Literal _ False) -> "0"
            ExpressionBound symbol -> symbolName symbol
            ExpressionCall symbol expressions _ ->
                symbolName symbol ++ " " ++ ppExpressions expressions
            _ -> error ("prettyPrint:" ++ show expression)
