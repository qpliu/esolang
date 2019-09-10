-- https://esolangs.org/wiki/Intramodular_Transaction

-- Build: ghc --make imtx
-- Usage: ./imtx SRC-FILE

import Data.Bits(testBit)
import Data.Char(chr,isAlpha,isAlphaNum,isSpace,ord)
import Data.List(elemIndex)
import Data.Map(Map,empty,insert,member,(!))
import qualified Data.Map
import System.Environment(getArgs)
import System.IO(char8,hSetEncoding,stdin,stdout)

tokenize :: String -> [String]
tokenize [] = []
tokenize (c:rest)
  | isSpace c = tokenize rest
  | c == '-' && take 1 rest == "-" = tokenize (dropWhile (/= '\n') rest)
  | c `elem` "01.?=;" = [c] : tokenize rest
  | isAlpha c = let (id,r) = break (not . isAlphaNum) rest
                in  (c:id) : tokenize r
  | otherwise = error ("Invalid token: " ++ [c])

isName :: String -> Bool
isName token = any isAlpha token

pass1 :: [String] -> Map String ([String],[String])
pass1 tokens = collectDefs empty tokens
  where
    collectDefs defs [] = defs
    collectDefs defs (name:tokens)
      | member name defs = error ("Duplicate definition: " ++ name)
      | isName name = collectDef defs name (break (not . isName) tokens)
      | otherwise = error ("Invalid definition: " ++ name)
    collectDef defs name (params,tokens)
      | take 1 tokens == ["="] =
            collectDefBody defs name params (break (== ";") (drop 1 tokens))
      | otherwise = error ("Invalid definition: " ++ name)
    collectDefBody defs name params (body,tokens)
      | take 1 tokens == [";"] =
            collectDefs (insert name (params,body) defs) (drop 1 tokens)
      | otherwise = error ("Invalid definition: " ++ name)

data Expr = Cons0 Expr | Cons1 Expr | Cdr Expr | Cond Expr Expr Expr | Arg Int | Call Expr [Expr]

pass2 :: Map String ([String],[String]) -> Map String Expr
pass2 def1s = def2s
  where
    arity name = length (fst (def1s ! name))
    def2s = Data.Map.map parseDef def1s
    parseDef (params,tokens) = let ([],expr) = parse params tokens in expr
    parse params (tok:tokens)
      | tok == "0" = fmap Cons0 (parse params tokens)
      | tok == "1" = fmap Cons1 (parse params tokens)
      | tok == "." = fmap Cdr (parse params tokens)
      | tok == "?" =
            let (toks1,expr1) = parse params tokens
                (toks2,expr2) = parse params toks1
                (toks3,expr3) = parse params toks2
            in  (toks3,Cond expr1 expr2 expr3)
      | otherwise = maybe (parseCall params tokens (arity tok) (def2s ! tok))
                          (((,) tokens) . Arg) (elemIndex tok params)
    parseCall params tokens argc expr =
        fmap (Call expr) (parseCallArgs params tokens argc)
    parseCallArgs params tokens argc
      | argc > 0 =
            let (toks1,expr) = parse params tokens
                (toks2,exprs) = parseCallArgs params toks1 (argc - 1)
            in  (toks2,expr:exprs)
      | otherwise = (tokens,[])

eval :: [[Bool]] -> Expr -> [Bool]
eval args (Cons0 expr) = False : eval args expr
eval args (Cons1 expr) = True : eval args expr
eval args (Cdr expr) = drop 1 (eval args expr)
eval args (Cond cond tExpr fExpr)
  | take 1 (eval args cond) == [True] = eval args tExpr
  | otherwise = eval args fExpr
eval args (Arg index) = args !! index
eval args (Call expr params) = eval (map (eval args) params) expr

run :: String -> String -> String
run src inp =
    decode (eval [encode inp] (pass2 (pass1 (tokenize src)) ! "main"))

encode ::  String -> [Bool]
encode str = concatMap encode8 str ++ repeat False
  where
    encode8 ch = concatMap ((True :) . (:[]) . (testBit (ord ch))) [0..7]

decode :: [Bool] -> String
decode (True:b0:True:b1:True:b2:True:b3:True:b4:True:b5:True:b6:True:b7:rest) =
    chr ((if b0 then 1 else 0) + (if b1 then 2 else 0) + (if b2 then 4 else 0)
        +(if b3 then 8 else 0) + (if b4 then 16 else 0)+ (if b5 then 32 else 0)
        +(if b6 then 64 else 0)+ (if b7 then 128 else 0)) : decode rest
decode _ = []

imtx :: String -> IO ()
imtx src =
    hSetEncoding stdin char8 >> hSetEncoding stdout char8 >> interact (run src)

main = getArgs >>= readFile . head >>= imtx
