-- Build: ghc --make pointless
-- Usage: ./pointless SRC-FILE

import Data.Char(chr,isSpace,ord)
import System.Environment(getArgs)
import System.IO(char8,hPutStrLn,hSetEncoding,stderr,stdin,stdout)

tokenize :: String -> [String]
tokenize "" = []
tokenize ('=':'=':rest) = tokenize (dropWhile (/= '\n') rest)
tokenize (c:rest)
  | isSpace c = tokenize rest
  | c `elem` "().,=" = [c] : tokenize rest
  | otherwise = tokenizeIdent [c] rest
  where
    tokenizeIdent ident "" = [reverse ident]
    tokenizeIdent ident (c:rest)
      | isSpace c || c `elem` "().,=" = reverse ident : tokenize (c:rest)
      | otherwise = tokenizeIdent (c:ident) rest

parse :: [String] -> Either String Expr
parse tokens = do
  (tokens,expr) <- parseExpr tokens
  case tokens of
    [] -> fail "Unexpected EOF"
    (".":_) -> return expr
    (token:_) -> fail ("Unexpected token: "++token)

parseExpr :: [String] -> Either String ([String],Expr)
parseExpr [] = fail "Unexpected EOF"
parseExpr ("(":tokens) = parseGrouping [] tokens
parseExpr ("LET":tokens) = parseLet [] tokens
parseExpr (token:tokens)
  | token `elem` [",",".","IN",")","="] = fail ("Unexpected token: "++token)
  | otherwise = parseCompose (Func token) tokens

parseCompose :: Expr -> [String] -> Either String ([String],Expr)
parseCompose expr [] = fail "Unexpected EOF"
parseCompose expr tokens@(token:_)
  | token `elem` [",",")","IN","."] = return (tokens,expr)
  | otherwise = do
      (newTokens,newExpr) <- parseExpr tokens
      return (newTokens,Compose expr newExpr)

parseGrouping :: [Expr] -> [String] -> Either String ([String],Expr)
parseGrouping _ [] = fail "Unexpected EOF"
parseGrouping exprs tokens = do
    (newTokens,newExpr) <- parseExpr tokens
    case (length exprs,newTokens) of
      (_,[]) -> fail "Unexpected EOF"
      (n,",":newNewTokens) ->
          if n < 2 then parseGrouping (newExpr:exprs) newNewTokens
                   else fail "Unexpected token: ,"
      (1,")":newNewTokens) -> parseCompose (Concat (head exprs) newExpr) newNewTokens
      (2,")":newNewTokens) -> parseCompose (If (head (tail exprs)) (head exprs) newExpr) newNewTokens
      (_,")":_) -> fail "Unexpected token: )"
      (_,token:_) -> fail ("Unexpected token: "++token)

parseLet :: [(String,Expr)] -> [String] -> Either String ([String],Expr)
parseLet bindings [] = fail "Unexpected EOF"
parseLet bindings (ident:"=":tokens)
  | ident `elem` [".",",","=","(",")","LET","IN"] = fail ("Unexpected token: "++ident)
  | otherwise = do
      (newTokens,expr) <- parseExpr tokens
      case newTokens of
        [] -> fail "Unexpected EOF"
        ("IN":newNewTokens) -> do
          (newNewNewTokens,newExpr) <- parseExpr newNewTokens
          return (newNewNewTokens,Let (reverse ((ident,expr):bindings)) newExpr)
        (",":newNewTokens) -> parseLet ((ident,expr):bindings) newNewTokens
        (token:_) -> fail ("Unexpected token: "++token)

data Expr =
    Compose Expr Expr
  | Let [(String,Expr)] Expr
  | If Expr Expr Expr
  | Concat Expr Expr
  | Func String

initialScope :: String -> Maybe ([Bool] -> Either String [Bool])
initialScope "_" = Just (const (return []))
initialScope "0" = Just (return . (False:))
initialScope "1" = Just (return . (True:))
initialScope _ = Nothing

resolve :: (String -> Maybe ([Bool] -> Either String [Bool])) -> Expr -> Either String ([Bool] -> Either String [Bool])
resolve scope (Compose expr1 expr2) = do
  f1 <- resolve scope expr1
  f2 <- resolve scope expr2
  return ((>>= f1) . f2)
resolve scope (Let bindings expr) = resolve newScope expr
  where
    newScope ident = maybe (scope ident) Just
                           (lookup ident (map (fmap bind) bindings))
    bind expr = either (const . fail) id (resolve newScope expr)
resolve scope (If expr1 expr2 expr3) = do
  f1 <- resolve scope expr1
  f2 <- resolve scope expr2
  f3 <- resolve scope expr3
  return (\ arg -> if null arg then f1 arg else if head arg then f3 (tail arg) else f2 (tail arg))
resolve scope (Concat expr1 expr2) = do
  f1 <- resolve scope expr1
  f2 <- resolve scope expr2
  return (\ arg -> do
    v1 <- f1 arg
    v2 <- f2 arg
    return (v1 ++ v2))
resolve scope (Func func) = maybe (fail ("Unbound identifier: "++func)) return (scope func)

interp :: Expr -> [Bool] -> Either String [Bool]
interp expr input = do
  fn <- resolve initialScope expr
  fn input

fromBits :: [Bool] -> String
fromBits bits
  | null bits = []
  | otherwise = chr (sum (zipWith bitVal (take 8 bits) [0..7])) : fromBits (drop 8 bits)
  where
    bitVal bit n = if bit then product (take n (repeat 2)) else 0

toBits :: String -> [Bool]
toBits = concatMap (byteBits 8 . ord)
  where
    byteBits b n | b <= 0 = []
                 | otherwise = (mod n 2 == 1) : byteBits (b-1) (div n 2)

run :: Expr -> IO ()
run expr = do
  hSetEncoding stdin char8
  hSetEncoding stdout char8
  interact (either id fromBits . interp expr . toBits)

main :: IO ()
main = do
  (srcFile:_) <- getArgs
  src <- readFile srcFile
  either (hPutStrLn stderr) run (parse (tokenize src))
