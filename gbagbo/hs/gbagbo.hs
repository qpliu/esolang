import Control.Monad(zipWithM)
import System.Environment(getArgs,getProgName)
import System.Exit(exitFailure)
import System.FilePath(dropExtensions,takeBaseName)
import System.IO(stderr,hPutStrLn)

import Ast(Def(..))
import Compile(compile)
import Encoding(fromString,toString)
import Eval(eval)
import Parse(parse)
import Resolve(resolve)
import Value(empty)

main :: IO ()
main = do
    args <- getArgs
    maybe usage interp (parseArgs args)

usage :: IO ()
usage = do
    progName <- getProgName
    die ("Usage:\n" ++ progName
                    ++ " [-m MAIN-FUNCTION] SOURCE-FILE [SOURCE-FILE...]")

die :: String -> IO a
die msg = do
    hPutStrLn stderr msg
    exitFailure

parseArgs :: [String] -> Maybe (String,[String])
parseArgs ("-m":mainFunc:srcFiles@(_:_)) = Just (mainFunc,srcFiles)
parseArgs srcFiles@(mainFile:_) = Just (getMainFunc mainFile,srcFiles)
parseArgs _ = Nothing

getMainFunc :: String -> String
getMainFunc = dropExtensions . takeBaseName

interp :: (String,[String]) -> IO ()
interp (mainFunc,srcFiles) = do
    srcCode <- mapM readFile srcFiles
    defs <- either (die . show) return (compile (do
                partials <- zipWithM parse srcFiles srcCode
                resolve (concat partials)))
    Def _ nargs expr <- maybe (die ("main function not found: " ++ mainFunc))
                              return (lookup mainFunc defs)
    args <- if nargs <= 0
                then return []
                else do
                    input <- getContents
                    return (take nargs (fromString input:repeat empty))
    putStr (toString (eval args expr))
