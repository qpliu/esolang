import System.Environment(getArgs)

import Compiler(compile)
import Parser(parse)

main :: IO ()
main = do
    args <- getArgs
    (sourceFiles,mainFunction,debug) <- parseArgs args
    either (error . show)
           (putStr . compile mainFunction debug)
           (parse sourceFiles)

parseArgs :: [String] -> IO ([(FilePath,String)],String,Bool)
parseArgs args = do
    (sourceFiles,mainFunction) <- readSources [] Nothing argsAfterFlags
    return (sourceFiles,maybe "main" id mainFunction,debug)
  where
    (argsAfterFlags,debug) = parseFlagArgs args
    parseFlagArgs ("-d":args) = (args,True)
    parseFlagArgs args = (args,False)
    readSources sourceFiles mainFunction [] = return (sourceFiles,mainFunction)
    readSources sourceFiles _ ("-":mainFunction:[]) =
        return (sourceFiles,Just mainFunction)
    readSources sourceFiles mainFunction (filename:args) = do
        source <- readFile filename
        readSources ((filename,source):sourceFiles)
                    (maybe (getMainFunction filename) Just mainFunction)
                    args
    getMainFunction filename =
        Just $ takeWhile (not . (`elem` ".01_="))
             $ reverse $ takeWhile (not . (`elem` "/\\")) $ reverse filename
