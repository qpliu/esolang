-- Build: ghc --make filo
-- Usage: ./filo SRC-FILE

import Data.Char(chr,isSpace,ord)
import Data.Map(Map,alter,empty,(!))
import System.Environment(getArgs)
import System.IO(char8,hSetEncoding,stdin,stdout)

tokenize :: String -> [String]
tokenize "" = []
tokenize ('=':'=':rest) = tokenize (dropWhile (/= '\n') rest)
tokenize (c:rest)
 | isSpace c = tokenize rest
 | not (isIdent [c]) = [c] : tokenize rest
 | otherwise = tokenizeIdent [c] rest
 where
   tokenizeIdent ident "" = [reverse ident]
   tokenizeIdent ident (c:rest)
     | isSpace c || not (isIdent [c]) = reverse ident : tokenize (c:rest)
     | otherwise = tokenizeIdent (c:ident) rest

isIdent :: String -> Bool
isIdent "" = error "Unexpected tokenize error"
isIdent [c] = not (c `elem` "@0*+-,=.")
isIdent _ = True

data Def = Def String Expr

data Expr = Arg | Nil | Push Expr Expr | Top Expr Expr | Pop Expr Expr
          | Apply String Expr | ResolvedApply Expr Expr

parse :: [String] -> [Def]
parse [] = []
parse tokens = def : parse rest where (def,rest) = parseDef tokens

parseDef :: [String] -> (Def,[String])
parseDef (ident:"=":tokens)
  | not (isIdent ident) = error ("Parse error: expected ident, got "++ident)
  | otherwise =
      case rest of
          (".":_) -> (Def ident expr,tail rest)
          _ -> error "Parse error: expected '.'"
  where (expr,rest) = parseExpr tokens
parseDef _ = error "Parse error: expected definition"

parseExpr :: [String] -> (Expr,[String])
parseExpr tokens = parseBinary expr rest where (expr,rest) = parseSingle tokens

parseSingle :: [String] -> (Expr,[String])
parseSingle ("@":tokens) = (Arg,tokens)
parseSingle ("0":tokens) = (Nil,tokens)
parseSingle (ident:tokens) | isIdent ident =
    case rest of
        (",":_) -> (Apply ident expr,tail rest)
        (".":_) -> (Apply ident expr,rest)
        _ -> error "Parse error: expected ',' or '.'"
  where (expr,rest) = parseExpr tokens
parseSingle _ = error "Parse error: expected '@', '0', or ident"

parseBinary :: Expr -> [String] -> (Expr,[String])
parseBinary lhs ("*":tokens) = (Push lhs rhs,rest) where (rhs,rest) = parseExpr tokens
parseBinary lhs ("+":tokens) = (Top lhs rhs,rest) where (rhs,rest) = parseExpr tokens
parseBinary lhs ("-":tokens) = (Pop lhs rhs,rest) where (rhs,rest) = parseExpr tokens
parseBinary expr tokens = (expr,tokens)

resolve :: [Def] -> Map String Expr
resolve defs = resolved
  where
    resolved = foldl resolve1 empty defs
    resolve1 m (Def ident expr) = alter resolveDef ident m
      where
        resolveDef Nothing = Just (resolveExpr expr)
        resolveDef _ = error ("Resolve error: multiple definitions: "++ident)
    resolveExpr Arg = Arg
    resolveExpr Nil = Nil
    resolveExpr (Push lhs rhs) = Push (resolveExpr lhs) (resolveExpr rhs)
    resolveExpr (Top lhs rhs) = Top (resolveExpr lhs) (resolveExpr rhs)
    resolveExpr (Pop lhs rhs) = Pop (resolveExpr lhs) (resolveExpr rhs)
    resolveExpr (Apply name body) = ResolvedApply (resolved!name) (resolveExpr body)
    resolveExpr (ResolvedApply _ _) = error "Unexpected resolve error"

data Val = Val [Val]

push :: Val -> Val -> Val
push val (Val vals) = Val (val:vals)

top :: Val -> Val -> Val
top (Val []) val = val
top (Val (val:_)) _ = val

pop :: Val -> Val -> Val
pop (Val []) val = val
pop (Val (_:vals)) _ = Val vals

eval :: Expr -> Val -> Val
eval Arg arg = arg
eval Nil arg = Val []
eval (Push lhs rhs) arg = push (eval lhs arg) (eval rhs arg)
eval (Top lhs rhs) arg = top (eval lhs arg) (eval rhs arg)
eval (Pop lhs rhs) arg = pop (eval lhs arg) (eval rhs arg)
eval (Apply _ _) arg = error "Unexpected eval error"
eval (ResolvedApply expr exprArg) arg = eval expr (eval exprArg arg)

serialize :: Val -> [Bool]
serialize (Val vals) = map (\ (Val v) -> not (null v)) vals

deserialize :: [Bool] -> Val
deserialize = Val . map (\ b -> if b then Val [Val []] else Val [])

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

main :: IO ()
main = do
  [srcFile] <- getArgs
  src <- readFile srcFile
  let mainFunc = takeWhile (/= '.') (reverse (takeWhile (/= '/') (reverse srcFile)))
      funcs = resolve (parse (tokenize src))
  hSetEncoding stdin char8
  hSetEncoding stdout char8
  interact (fromBits . serialize . eval (funcs!mainFunc) . deserialize . toBits)
