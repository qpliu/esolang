-- https://esolangs.org/wiki/Unarian

-- Build: ghc --make junior
-- Usage: ./junior [-input N] [-main MAIN] SOURCE-FILE
-- N defaults to 0
-- MAIN defaults to main

import Control.Monad(foldM)
import Data.Char(isSpace)
import Data.Map(Map,fromList,insert,member,(!))
import qualified Data.Map
import System.Environment(getArgs,getProgName)

tokenize :: String -> [String]
tokenize [] = []
tokenize (c:rest)
  | isSpace c = tokenize rest
  | c == '#' = tokenize $ dropWhile (/= '\n') rest
  | otherwise = ident [c] rest
  where
    ident i [] = [reverse i]
    ident i (c:rest)
      | isSpace c = reverse i:tokenize rest
      | c == '#' = reverse i:tokenize (dropWhile (/= '\n') rest)
      | otherwise = ident (c:i) rest

data Program = Apply String | Compose [Program] | Branch Program Program

parse :: [String] -> Either String [(String,Program)]
parse tokens = parseDefs [] tokens
  where
    parseDefs :: [(String,Program)] -> [String] -> Either String [(String,Program)]
    parseDefs defs [] = Right defs
    parseDefs defs ("{":_) = Left "Syntax error: expected function definition"
    parseDefs defs ("}":_) = Left "Syntax error: expected function definition"
    parseDefs defs ("|":_) = Left "Syntax error: expected function definition"
    parseDefs defs (name:"{":toks) = do
      (defBody,moreToks) <- parseBody [] toks
      parseDefs ((name,Compose defBody):defs) moreToks
    parseDefs defs _ = Left "Syntax error: expected function definition"
    parseBody :: [Program] -> [String] -> Either String ([Program],[String])
    parseBody p [] = Left "Syntax error: unexpected EOF"
    parseBody p ("{":toks) = do
      (block,moreToks) <- parseBody [] toks
      parseBody (Compose block:p) moreToks
    parseBody p ("}":toks) = Right (reverse p,toks)
    parseBody p ("|":toks) = do
      (branch,moreToks) <- parseBody [] toks
      Right ([Branch (Compose (reverse p)) (Compose (reverse branch))],moreToks)
    parseBody p (tok:toks) = parseBody (Apply tok:p) toks

resolve :: [(String,Program)] -> Either String (Map String (Integer -> Maybe Integer))
resolve defs = resolution
  where
    resolution :: Either String (Map String (Integer -> Maybe Integer))
    resolution = foldM addDef (fromList [("+",increment),("-",decrement),("!",debug1),("?",debug2)]) defs
    addDef :: Map String (Integer -> Maybe Integer) -> (String,Program) -> Either String (Map String (Integer -> Maybe Integer))
    addDef table (name,body)
      | name `member` table = Left ("Redefined: " ++ name)
      | otherwise = Right (insert name (eval definitions body) table)
    definitions :: Map String (Integer -> Maybe Integer)
    Right definitions = resolution

increment :: Integer -> Maybe Integer
increment n = Just (n + 1)

decrement :: Integer -> Maybe Integer
decrement n | n <= 0 = Nothing | otherwise = Just (n - 1)

debug1 :: Integer -> Maybe Integer
debug1 n = Just n

debug2 :: Integer -> Maybe Integer
debug2 n = Just n

eval :: Map String (Integer -> Maybe Integer) -> Program -> Integer -> Maybe Integer
eval defs (Apply function) n = (defs!function) n -- (!) fails for undefined functions
eval defs (Compose []) n = Just n
eval defs (Compose (p:ps)) n = maybe Nothing (eval defs (Compose ps)) (eval defs p n)
eval defs (Branch f g) n = maybe (eval defs g n) Just (eval defs f n)

parseArgs :: String -> [String] -> Either String (Integer,String,String)
parseArgs progName args = p (0,"main") args
  where
    p (n,mainName) [] = Left usage
    p (n,mainName) [src] = Right (n,mainName,src)
    p (_,mainName) ("-input":n:args) = p (read n,mainName) args
    p (n,_) ("-main":mainName:args) = p (n,mainName) args
    p _ _ = Left usage
    usage = "Usage: " ++ progName ++ " [-input N] [-main MAIN] SOURCE-FILE"

run :: (Integer,String,String) -> IO (Either String (Maybe Integer))
run (input,mainName,srcFile) = do
    src <- readFile srcFile
    return (fmap interp (parse (tokenize src) >>= resolve))
  where
    interp defs = (defs!mainName) input

main :: IO ()
main = do
    progName <- getProgName
    args <- getArgs
    result <- either (return . Left) run (parseArgs progName args)
    either putStrLn (maybe (putStrLn "fail") print) result
