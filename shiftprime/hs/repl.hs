import Data.Map(Map)
import qualified Data.Map as M
import System.Environment(getArgs)
import System.IO(hFlush,stdout)

import Tokenize(Location(Location),tokenize,Token(Equals,Dot))
import Parse(parseDef1,parseStream,def1Arity,Def1,Def,
             compileExpr,Expr(ExprCompileError),compile)
import Eval(eval,fromVal)

main :: IO ()
main = do
    args <- getArgs
    parseResult <- parseFiles M.empty args
    either (mapM_ (putStrLn . formatError)) (repl 0 []) parseResult

parseFiles :: Map String Def1 -> [String] -> IO (Either [(Location,String)] (Map String Def1))
parseFiles def1s [] = return (Right def1s)
parseFiles def1s (file:files) = do
    fileContents <- readFile file
    case parseStream def1s (tokenize file fileContents) of
      ([],def1s') -> parseFiles def1s' files
      (errors,_) -> return (Left errors)

formatError :: (Location,String) -> String
formatError (Location name line col,msg) =
    name ++ ":" ++ show line ++ ":" ++ show col ++ ": " ++ msg

repl :: Int -> [Token] -> Map String Def1 -> IO ()
repl level [] def1s = do
    prompt 0
    line <- getLine
    repl 1 (tokenize (inputName 0) line) def1s
repl level tokens def1s
  | hasDot tokens && hasEquals tokens =
        let (errors,def1s') = parseStream def1s tokens
        in  do
                mapM_ (putStrLn . formatError) errors
                repl 0 [] def1s'
  | hasEquals tokens = do
        prompt level
        line <- getLine
        repl (level + 1) (tokenize (inputName level) line) def1s
  | hasDot tokens =
        case eval (flip M.lookup (compile def1s)) (compileExpr (def1Arity def1s) tokens) of
          Left err -> do
            putStrLn (formatError err)
            repl 0 [] def1s
          Right result -> do
            print (fromVal result)
            repl 0 [] def1s
  | otherwise =
        case eval (flip M.lookup (compile def1s)) (compileExpr (def1Arity def1s) tokens) of
          Left err -> do
            prompt level
            line <- getLine
            repl (level + 1) (tokenize (inputName level) line) def1s
          Right result -> do
            print (fromVal result)
            repl 0 [] def1s

hasEquals :: [Token] -> Bool
hasEquals [] = False
hasEquals (Equals _:_) = True
hasEquals (_:tokens) = hasEquals tokens

hasDot :: [Token] -> Bool
hasDot [] = False
hasDot (Dot _:_) = True
hasDot (_:tokens) = hasDot tokens

inputName :: Int -> String
inputName 0 = "(stdin)"
inputName n = "(stdin:" ++ show (n+1) ++ ")"

prompt :: Int -> IO ()
prompt 0 = putStr "> " >> hFlush stdout
prompt n = putStr (show (n+1)) >> putStr "> " >> hFlush stdout
