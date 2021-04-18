-- https://esolangs.org/wiki/Μ

-- Build: ghc --make moo
-- Usage: ./moo

-- The μ syntax is underspecified.

-- I'll assume that two consecutive newlines (i.e. a blank line) ends a
-- definition.

-- I'll assume identifiers are non-empty strings of non-whitespace characters
-- excluding parentheses, but cannot be "=".

-- I'll assume that definitions where the identifier being defined is within
-- parentheses are defined as infix (or postfix for unary functions).

-- I'll assume that expressions invoking infix functions have the function
-- between the first and second arguments, with additional arguments, if any,
-- following the second argument.

-- I'll assume infix functions are right associative.

-- Not exactly syntax, but I'll assume redefining <= is disallowed.

-- Additionally, if there is an "=" token in the first line of an entry, it is
-- considered a definition that can span multiple lines and is ended by a
-- blank line.  And if there is no "=" token in the first line, it is
-- considered an expression that will be evaluated and cannot span multiple
-- lines.

-- I don't see how evaluating recursive functions such as
-- (+) a b c = ((b == 0) && (c == a)) || (c == inc (a + dec b))
-- terminates without some short-circuiting mechanism.
-- I can see two short-circuit mechanisms:
--   1. If the left argument of <= is 0, the result is 1.
--   2. If the left argument of <= is 2 or greater and the right argument
--      can be statically proven to be either 0 or 1, the result is 0.
-- I don't think these are sufficient to make the evaluation of + terminate.

import Data.Char(isSpace)
import Data.Map(Map,empty,insert,member,toList,(!))
import qualified Data.Map

tokenize :: String -> [String]
tokenize [] = []
tokenize (c:rest)
  | c == '(' || c == ')' = [c] : tokenize rest
  | isSpace c = tokenize rest
  | otherwise = let (ident,remaining) = break nonIdent rest
                in  (c:ident) : tokenize remaining
  where
    nonIdent c = c == '(' || c == ')' || isSpace c

data Token = Identifier String | Grouping [Token] | Argument Int | Implicit
  deriving Show

data Unresolved = Unresolved Bool Int [Token] -- infix arity body
  deriving Show

parseDef :: [String] -> Either String (String,Unresolved)
parseDef tokens = parseDefName tokens
  where
    parseDefName ("(":name:")":tokens)
      | name `elem` ["(",")","=","<="] = Left "Bad definition"
      | otherwise = fmap ((,) name) (parseDefParams True tokens)
    parseDefName (name:tokens)
      | name `elem` ["(",")","=","<="] = Left "Bad definition"
      | otherwise = fmap ((,) name) (parseDefParams False tokens)
    parseDefName [] = Left "Bad definition"
    parseDefParams :: Bool -> [String] -> Either String Unresolved
    parseDefParams infixFlag (name:tokens)
      | name `elem` ["(",")","=","<="] = Left "Bad definition"
      | otherwise = parseDefAdditionalParams infixFlag name [] tokens
    parseDefParams infixFlag [] = Left "Bad definition"
    parseDefAdditionalParams :: Bool -> String -> [String] -> [String] -> Either String Unresolved
    parseDefAdditionalParams infixFlag implicit params (name:tokens)
      | name `elem` ["(",")","<="] = Left "Bad definition"
      | name == "=" = fmap (Unresolved infixFlag (length params)) (parseExpr (zip (reverse params) [0..]) implicit tokens)
      | otherwise = parseDefAdditionalParams infixFlag name (implicit:params) tokens
    parseDefAdditionalParams infixFlag implicit params [] = Left "Bad definition"

parseExpr :: [(String,Int)] -> String -> [String] -> Either String [Token]
parseExpr params implicit tokens = do
    (exprTokens,remainingTokens) <- parseTokens [] tokens
    if null remainingTokens
      then return exprTokens
      else Left "Unmatched parenthesis"
  where
    parseTokens :: [Token] -> [String] -> Either String ([Token],[String])
    parseTokens parsedToks [] = return (reverse parsedToks,[])
    parseTokens parsedToks toks@(")":_) = return (reverse parsedToks,toks)
    parseTokens parsedToks ("(":rest) = do
      (groupedToks,remaining) <- parseTokens [] rest
      case remaining of
        (")":rest) -> parseTokens (Grouping groupedToks:parsedToks) rest
        _ -> Left "Unmatched parenthesis"
    parseTokens parsedToks (name:rest)
      | name == implicit = parseTokens (Implicit:parsedToks) rest
      | otherwise = parseTokens (maybe (Identifier name) Argument (lookup name params):parsedToks) rest

data Expr = Call (Either String Expr) [Expr] | Arg Int | Impl | LE Expr Expr

resolve :: Map String Unresolved -> [Token] -> Either String Expr
resolve unresolved tokens = resolveExpr tokens
  where
    isInfix :: String -> Bool
    isInfix name = (\ (Unresolved infixFlag _ _) -> infixFlag) (unresolved!name)
    arity :: String -> Int
    arity name = (\ (Unresolved _ a _) -> a) (unresolved!name)
    resolved :: Map String (Either String Expr)
    resolved = Data.Map.map resolveDef unresolved
    resolveDef :: Unresolved -> Either String Expr
    resolveDef (Unresolved _ _ tokens) = resolveExpr tokens
    resolveExpr :: [Token] -> Either String Expr
    resolveExpr tokens = do
        (exprList,remainingToks) <- resolveExprList ([],tokens) 1 False
        if not (null remainingToks)
          then Left "Resolver failure"
          else return (head exprList)
    resolveExprList :: ([Expr],[Token]) -> Int -> Bool -> Either String ([Expr],[Token])
    resolveExprList (exprStack,[]) n stopOnInfix
      | length exprStack == n = return (reverse exprStack,[])
      | otherwise = Left "Resolver failure"
    resolveExprList (exprStack,toks@(Identifier "<=":remainingToks)) n stopOnInfix
      | null exprStack = Left "Resolver failure"
      | stopOnInfix && length exprStack == n = return (reverse exprStack,toks)
      | otherwise = do
          (rhs,moreToks) <- resolveExprList ([],remainingToks) 1 False
          resolveExprList (LE (head exprStack) (head rhs):tail exprStack,moreToks) n stopOnInfix
    resolveExprList (exprStack,toks@(Identifier name:remainingToks)) n stopOnInfix
      | not (member name unresolved) = Left "Resolver failure"
      | (arity name == 0 || not (isInfix name) || stopOnInfix) && length exprStack == n = return (reverse exprStack,toks)
      | arity name == 0 =
          resolveExprList (Call (resolved!name) []:exprStack,remainingToks) n stopOnInfix
      | isInfix name && null exprStack = Left "Resolver failure"
      | isInfix name && arity name == 1 =
          resolveExprList (Call (resolved!name) [head exprStack]:tail exprStack,remainingToks) n stopOnInfix
      | isInfix name = do
          (args,moreToks) <- resolveExprList ([],remainingToks) (arity name - 1) False
          resolveExprList (Call (resolved!name) (head exprStack:args):tail exprStack,moreToks) n stopOnInfix
      | otherwise = do
          (args,moreToks) <- resolveExprList ([],remainingToks) (arity name) True
          resolveExprList (Call (resolved!name) args:exprStack,moreToks) n stopOnInfix
    resolveExprList (exprStack,toks@(Grouping group:remainingToks)) n stopOnInfix
      | length exprStack == n = return (reverse exprStack,toks)
      | otherwise = do
          expr <- resolveExpr group
          resolveExprList (expr:exprStack,remainingToks) n stopOnInfix
    resolveExprList (exprStack,toks@(Argument i:remainingToks)) n stopOnInfix
      | length exprStack == n = return (reverse exprStack,toks)
      | otherwise = resolveExprList (Arg i:exprStack,remainingToks) n stopOnInfix
    resolveExprList (exprStack,toks@(Implicit:remainingToks)) n stopOnInfix
      | length exprStack == n = return (reverse exprStack,toks)
      | otherwise = resolveExprList (Impl:exprStack,remainingToks) n stopOnInfix

eval :: Expr -> [Integer] -> Integer -> Either String Integer
eval (Call (Left msg) args) params implicit = Left msg
eval (Call (Right fn) args) params implicit = call 0
  where evalArg arg = eval arg params implicit
        call i = do
          callArgs <- mapM evalArg args
          r <- eval fn callArgs i
          if r == 0
            then call (i+1)
            else return i
eval (Arg i) params implicit = return (params !! i)
eval Impl params implicit = return implicit
eval (LE lhs rhs) params implicit = do
    l <- eval lhs params implicit
    evalLE l
  where
    evalLE l
      | l == 0 = return 1
      | l > 1 && isLE rhs = return 0
      | otherwise = do
          r <- eval rhs params implicit
          return (if l <= r then 1 else 0)
    isLE (LE _ _) = True
    isLE _ = False

repl :: (Map String Unresolved,[String]) -> IO ()
repl (defs,tokens) = do
    putStr (if null tokens then "- " else "+ ")
    line <- getLine
    epl (tokenize line)
  where
    epl []
      | null tokens = repl (defs,[])
      | otherwise = either perror addDef (parseDef tokens)
    epl [":q"] = return ()
    epl [":?"] = do
        putStrLn ":? - print this message"
        putStrLn ":d - print definitions"
        putStrLn ":c - clear definitions"
        putStrLn ":l file - load definitions from file"
        putStrLn ":q - quit"
        repl (defs,[])
    epl [":d"] = do
        mapM_ print (toList defs)
        repl (defs,[])
    epl [":c"] = repl (empty,[])
    epl [":l",file] = do
        src <- readFile file
        let (errors,newDefs) = load [] defs [] (lines src)
        mapM_ putStrLn errors
        repl (newDefs,[])
    epl toks
      | not (null tokens) = repl (defs,tokens++toks)
      | "=" `elem` toks = repl (defs,toks)
      | otherwise = do
            either perror print (compileEval toks)
            repl (defs,[])
    perror msg = do
        putStrLn msg
        repl (defs,[])
    addDef (name,def) = repl (insert name def defs,[])
    compileEval toks = do
        tokens <- parseExpr [] "" toks
        expr <- resolve defs tokens
        eval expr [] 0
    load errors defs tokens lines
      | null tokens && null lines = (reverse errors,defs)
      | null lines || null (tokenize (head lines)) =
          either (\ msg -> load (msg:errors) defs [] (drop 1 lines))
                 (\ (name,def) -> load errors (insert name def defs) [] (drop 1 lines))
                 (parseDef tokens)
      | otherwise = load errors defs (tokens ++ tokenize (head lines)) (tail lines)

main :: IO ()
main = do
    putStrLn "μ: https://esolangs.org/wiki/Μ  :? for help"
    repl (empty,[])
