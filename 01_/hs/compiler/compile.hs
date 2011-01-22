import System.Environment(getArgs)

import Compiler(compile)
import Parser(parse)

main :: IO ()
main = do
    args <- getArgs
    (sourceFiles,mainFunction) <- parseArgs args
    either (error . show) (putStr . compile mainFunction) (parse sourceFiles)

parseArgs :: [String] -> IO ([(FilePath,String)],String)
parseArgs args = do
    (sourceFiles,mainFunction) <- readSources [] Nothing args
    return (sourceFiles,maybe "main" id mainFunction)
  where
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
