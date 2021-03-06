-- Build: ghc --make filo
-- Usage: ./filo SRC-FILE

import Data.Char(chr,isSpace,ord)
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
isIdent [c] = not (c `elem` "@0*+-=,[]")
isIdent _ = True

data Expr = Arg | Nil | Push Expr Expr | Top Expr Expr | Pop Expr Expr | Let [(String,Expr)] Expr | Apply String String Expr Expr | Apply1 String Expr

parse :: [String] -> Expr
parse tokens = fst (parseExpr tokens)

parseExpr :: [String] -> (Expr,[String])
parseExpr tokens = parseBinary expr rest where (expr,rest) = parseSingle tokens

parseSingle :: [String] -> (Expr,[String])
parseSingle [] = error "Parse error: unexpected EOF"
parseSingle ("@":tokens) = (Arg,tokens)
parseSingle ("0":tokens) = (Nil,tokens)
parseSingle ("[":tokens) = parseLet [] tokens
parseSingle (ident1:"*":ident2:tokens) | isIdent ident1 && isIdent ident2 = parseApply ident1 ident2 tokens
parseSingle (token:_) = error ("Parse error: unexpected token: "++token)

parseApply :: String -> String -> [String] -> (Expr,[String])
parseApply ident1 ident2 tokens =
    case parseExpr tokens of
        (arg,(",":rest)) ->
            case parseExpr rest of
                (nil,(",":remaining)) -> (Apply ident1 ident2 arg nil,remaining)
                (nil,remaining) -> (Apply ident1 ident2 arg nil,remaining)
        (_,token:_) -> error ("Parse error: unexpected token: "++token)
        (_,[]) -> error "Parse error: unexpected EOF"

parseLet :: [(String,Expr)] -> [String] -> (Expr,[String])
parseLet defs [] = error "Parse error: unexpected EOF"
parseLet defs ("]":tokens) = (Let defs body,rest)
  where (body,rest) = parseExpr tokens
parseLet defs (ident:"=":tokens) | isIdent ident = parseLet newDefs rest
  where
    (defBody,rest) = parseExpr tokens
    (letBody,remaining) = parseExpr (tail rest)
    newDefs = maybe ((ident,defBody):defs) (const (error ("Parse error: multiple definitions: "++ident))) (lookup ident defs)
parseLet defs (token:_) = error ("Parse error: unexpected token: "++token)

parseBinary :: Expr -> [String] -> (Expr,[String])
parseBinary lhs ("*":tokens) = (Push lhs rhs,rest) where (rhs,rest) = parseExpr tokens
parseBinary lhs ("+":tokens) = (Top lhs rhs,rest) where (rhs,rest) = parseExpr tokens
parseBinary lhs ("-":tokens) = (Pop lhs rhs,rest) where (rhs,rest) = parseExpr tokens
parseBinary expr tokens = (expr,tokens)

optimize :: Expr -> Expr
optimize (Apply _ _ Nil expr) = optimize expr
optimize (Pop (Apply _ f (Push _ expr) _) _) = Apply1 f (optimize expr)
optimize Arg = Arg
optimize Nil = Nil
optimize (Push lhs rhs) = Push (optimize lhs) (optimize rhs)
optimize (Top lhs rhs) = Top (optimize lhs) (optimize rhs)
optimize (Pop lhs rhs) = Pop (optimize lhs) (optimize rhs)
optimize (Let bindings expr) = Let (map (fmap optimize) bindings) (optimize expr)
optimize (Apply f g arg nil) = Apply f g (optimize arg) (optimize nil)
optimize (Apply1 f expr) = error "Internal error: optimize Apply1 should be unreachable"

data RExpr = RArg | RNil | RPush RExpr RExpr | RTop RExpr RExpr | RPop RExpr RExpr | RApply RExpr RExpr RExpr RExpr | RApply1 RExpr RExpr

resolve :: [[(String,RExpr)]] -> Expr -> RExpr
resolve scopes expr = case expr of
    Arg -> RArg
    Nil -> RNil
    (Push lhs rhs) -> RPush (resolve scopes lhs) (resolve scopes rhs)
    (Top arg nil) -> RTop (resolve scopes arg) (resolve scopes nil)
    (Pop arg nil) -> RPop (resolve scopes arg) (resolve scopes nil)
    (Let defs body) -> let scope = map (fmap (resolve (scope:scopes))) defs in resolve (scope:scopes) body
    (Apply f g arg nil) -> RApply (lookupDef scopes f) (lookupDef scopes g) (resolve scopes arg) (resolve scopes nil)
    (Apply1 f arg) -> RApply1 (lookupDef scopes f) (resolve scopes arg)
  where
    lookupDef [] f = error ("Resolve error: undefined symbol: "++f)
    lookupDef (scope:outerScopes) f = maybe (lookupDef outerScopes f) id (lookup f scope)

data Val = Val [Val] deriving Show

push :: Val -> Val -> Val
push val (Val vals) = Val (val:vals)

top :: Val -> Val -> Val
top (Val []) val = val
top (Val (val:_)) _ = val

pop :: Val -> Val -> Val
pop (Val []) val = val
pop (Val (_:vals)) _ = Val vals

apply :: (Val -> Val) -> (Val -> Val) -> Val -> Val -> Val
apply f g (Val []) val = val
apply f g (Val (val:vals)) _ = push (f val) (g (Val vals))

eval :: RExpr -> Val -> Val
eval RArg arg = arg
eval RNil arg = Val []
eval (RPush lhs rhs) arg = push (eval lhs arg) (eval rhs arg)
eval (RTop expr nil) arg = top (eval expr arg) (eval nil arg)
eval (RPop expr nil) arg = pop (eval expr arg) (eval nil arg)
eval (RApply f g expr nil) arg = apply (eval f) (eval g) (eval expr arg) (eval nil arg)
eval (RApply1 f expr) arg = eval f (eval expr arg)

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

run :: String -> Val -> IO Val
run srcFile val =
  fmap (flip eval val . resolve [] . optimize . parse . tokenize) (readFile srcFile)

main :: IO ()
main = do
  [srcFile] <- getArgs
  expr <- fmap (resolve [] . optimize . parse . tokenize) (readFile srcFile)
  hSetEncoding stdin char8
  hSetEncoding stdout char8
  interact (fromBits . serialize . eval expr . deserialize . toBits)
