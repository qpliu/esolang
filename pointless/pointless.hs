-- Build: ghc --make pointless
-- Usage: ./pointless SRC-FILE

import Data.Char(chr,isSpace,ord)
import System.Environment(getArgs)
import System.IO(char8,hSetEncoding,stdin,stdout)

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

parse :: [String] -> Expr
parse tokens =
  case parseExpr tokens of
    ([],_) -> error "Unexpected EOF"
    (".":_,expr) -> expr
    (token:_,_) -> error ("Unexpected token: "++token)

parseExpr :: [String] -> ([String],Expr)
parseExpr [] = error "Unexpected EOF"
parseExpr ("(":tokens) = parseGrouping [] tokens
parseExpr ("LET":tokens) = parseLet [] tokens
parseExpr (token:tokens)
  | token `elem` [",",".","IN",")","="] = error ("Unexpected token: "++token)
  | otherwise = parseCompose (Func token) tokens

parseCompose :: Expr -> [String] -> ([String],Expr)
parseCompose expr [] = error "Unexpected EOF"
parseCompose expr tokens@(token:_)
  | token `elem` [",",")","IN","."] = (tokens,expr)
  | otherwise = let (newTokens,newExpr) = parseExpr tokens
                in  (newTokens,Compose expr newExpr)

parseGrouping :: [Expr] -> [String] -> ([String],Expr)
parseGrouping _ [] = error "Unexpected EOF"
parseGrouping exprs tokens =
    case parseExpr tokens of
      ([],_) -> error "Unexpected EOF"
      (",":newTokens,expr) ->
        if length exprs < 2 then parseGrouping (expr:exprs) newTokens
                            else error "Unexpected token: ,"
      (")":newTokens,expr) ->
        if length exprs == 1
          then parseCompose (Concat (head exprs) expr) newTokens
          else if length exprs == 2
            then parseCompose (If (head (tail exprs)) (head exprs) expr) newTokens
            else error "Unexpected token: )"
      (token:_,_) -> error ("Unexpected token: "++token)

parseLet :: [(String,Expr)] -> [String] -> ([String],Expr)
parseLet bindings [] = error "Unexpected EOF"
parseLet bindings (ident:"=":tokens)
  | ident `elem` [".",",","=","(",")","LET","IN"] = error ("Unexpected token: "++ident)
  | otherwise =
      case parseExpr tokens of
        ([],_) -> error "Unexpected EOF"
        ("IN":newTokens,expr) ->
            let (newNewTokens,letExpr) = parseExpr newTokens
            in  (newNewTokens,Let (reverse ((ident,expr):bindings)) letExpr)
        (",":newTokens,expr) -> parseLet ((ident,expr):bindings) newTokens
        (token:_,_) -> error ("Unexpected token: "++token)
parseLet _ _ = error "Invalid LET binding"

data Expr =
    Compose Expr Expr
  | Let [(String,Expr)] Expr
  | If Expr Expr Expr
  | Concat Expr Expr
  | Func String

initialScope :: String -> Maybe ([Bool] -> [Bool])
initialScope "_" = Just (const [])
initialScope "0" = Just (False:)
initialScope "1" = Just (True:)
initialScope _ = Nothing

resolve :: (String -> Maybe ([Bool] -> [Bool])) -> Expr -> ([Bool] -> [Bool])
resolve scope (Compose expr1 expr2) = resolve scope expr1 . resolve scope expr2
resolve scope (Let bindings expr) = resolve newScope expr
  where
    newScope ident = maybe (scope ident) Just
                           (lookup ident (map (fmap (resolve newScope)) bindings))
resolve scope (If expr1 expr2 expr3) = (\ arg ->
    if null arg
      then resolve scope expr1 arg
      else resolve scope (if head arg then expr3 else expr2) (tail arg))
resolve scope (Concat expr1 expr2) = (\ arg ->
    resolve scope expr1 arg ++ resolve scope expr2 arg)
resolve scope (Func func) = maybe (error ("Unbound identifier: "++func)) id (scope func)

interp :: Expr -> [Bool] -> [Bool]
interp expr input = (resolve initialScope expr) input

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

to01String :: [Bool] -> String
to01String = map (\ b -> if b then '1' else '0')

from01String :: String -> [Bool]
from01String = concatMap (\ ch -> if ch == '0' then [False] else if ch == '1' then [True] else [])

main :: IO ()
main = do
  args <- getArgs
  let (srcFile,fromStr,toStr) =
        case args of
            [srcFile] -> (srcFile,toBits,fromBits)
            ["-b",srcFile] -> (srcFile,from01String,to01String)
            _ -> error "Usage: pointless [-b] SRCFILE"
  src <- readFile srcFile
  hSetEncoding stdin char8
  hSetEncoding stdout char8
  interact (toStr . interp (parse (tokenize src)) . fromStr)
