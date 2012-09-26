import Data.Char(chr,ord)
import Data.Map(Map)
import qualified Data.Map as M
import System.Environment(getArgs)

import Tokenize(Location(Location),tokenize)
import Parse(parseStream,Def1,Def(Def),compile)
import Eval(evalFuncall,toVal,fromVal)

main :: IO ()
main = do
    args <- getArgs
    result <- handleArgs args
    either (mapM_ putStrLn) interp result

handleArgs :: [String] -> IO (Either [String] (String,Map String Def))
handleArgs [] = return (Left ["Usage: interp SRCFILE [SRCFILE ...] [- MAIN-FUNCTION]"])
handleArgs ["-"] = return (Left ["Usage: interp SRCFILE [SRCFILE ...] [- MAIN-FUNCTION]"])
handleArgs ["-",_] = return (Left ["Usage: interp SRCFILE [SRCFILE ...] [- MAIN-FUNCTION]"])
handleArgs args@(arg:_) = handleArgs' M.empty (fromFilename arg) args
  where
    fromFilename s =
        takeWhile (/= '.') (reverse (takeWhile (/= '/') (reverse s)))
    handleArgs' def1s _ ["-",mainFunc] = handleArgs' def1s mainFunc []
    handleArgs' def1s mainFunc [] =
        return (Right (mainFunc,compile def1s))
    handleArgs' def1s mainFunc (arg:args) = do
        fileContents <- readFile arg
        case parseStream def1s (tokenize arg fileContents) of
            ([],def1s') -> handleArgs' def1s' mainFunc args
            (errors,_) -> return (Left (map formatError errors))

interp :: (String,Map String Def) -> IO ()
interp (mainFunc,defs) =
    case M.lookup mainFunc defs of
      Nothing -> putStrLn ("Function not defined: "++mainFunc)
      Just def@(Def _ _ 0 _) ->
        either (putStrLn . formatError) (putStr . valToString)
            (evalFuncall (flip M.lookup defs) [] def)
      Just def@(Def _ _ n _) -> do
        args <- fmap ((:repeat []) . valFromString) getContents
        either (putStrLn . formatError) (putStr . valToString)
            (evalFuncall (flip M.lookup defs) (take n args) def)

formatError :: (Location,String) -> String
formatError (Location name line col,msg) =
    name ++ ":" ++ show line ++ ":" ++ show col ++ ": " ++ msg

valFromString :: String -> [Integer]
valFromString str = toVal (numFromString str 1 0)
  where
    numFromString [] base acc = acc + base
    numFromString (c:cs) base acc =
        numFromString cs (base*256) (acc + base*fromIntegral (ord c))

valToString :: [Integer] -> String
valToString val = numToString (fromVal val)
  where
    numToString 0 = ""
    numToString 1 = ""
    numToString n =
        let (n',c) = divMod n 256 in chr (fromIntegral c):numToString n'
