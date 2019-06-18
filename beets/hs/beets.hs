import Data.Map(Map)
import qualified Data.Map
import System.Environment(getArgs,getProgName)
import System.Exit(exitFailure)
import System.FilePath(dropExtensions,takeBaseName)
import System.IO(char8,hPutStr,hSetEncoding,stderr,stdin,stdout)


import Ast(Function(Function))
import Encoding(fromString,toString)
import Eval(eval)
import Parse(parse)
import Tokenize(tokenize)
import Tree(Tree,cons)

main :: IO ()
main = getArgs >>= maybe usage interp . parseArgs

usage :: IO ()
usage = do
    progName <- getProgName
    die ("Usage:\n" ++ progName ++ " [-m MAIN-FUNCTION] SOURCE-FILE\n")

die :: String -> IO a
die msg = hPutStr stderr msg >> exitFailure

parseArgs :: [String] -> Maybe (String,String)
parseArgs ["-m",mainFunc,srcFile] = Just (mainFunc,srcFile)
parseArgs [srcFile] = Just (dropExtensions $ takeBaseName srcFile,srcFile)
parseArgs _ = Nothing

interp :: (String,String) -> IO ()
interp (mainFunc,srcFile) = do
    srcCode <- readFile srcFile
    funcs <- either (die . unlines) return (parse (tokenize srcFile srcCode))
    Function _ params expr <- maybe
                    (die ("main function not defined: " ++ mainFunc ++ "\n"))
                    return
                    (Data.Map.lookup mainFunc funcs)
    hSetEncoding stdin char8
    args <- readArgs params
    hSetEncoding stdout char8
    putStr $ toString $ eval args expr

readArgs :: [a] -> IO [Tree]
readArgs [] = return []
readArgs (_:params) = fmap ((: map (const z) params) . fromString) getContents

z :: Tree
z = cons False z z
