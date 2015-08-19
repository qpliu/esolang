import Data.Map((!))
import System.Environment(getArgs)

import Eval(evalCall,numToVal,valToNum)
import Parse(Fn,arity,parse)

main :: IO ()
main = do
    (srcFiles,mainFn,args) <- fmap parseArgs getArgs
    fns <- fmap (parse . concat) (mapM readFile srcFiles)
    putStrLn (either id (show . interp args . (!mainFn)) fns)

interp :: [Integer] -> Fn -> Integer
interp args fn =
    valToNum (evalCall fn (take (arity fn) (map numToVal (args ++ repeat 0))))

-- SRCFILE [SRCFILE...] [- MAINFN] [-- ARG [ARG...]]
parseArgs :: [String] -> ([String],String,[Integer])
parseArgs (src:args) = gatherSrcs [src] (defaultMain src) args
  where
    gatherSrcs srcs mainFn [] = (reverse srcs,mainFn,[])
    gatherSrcs srcs _ ("-":mainFn:[]) = (reverse srcs,mainFn,[])
    gatherSrcs srcs _ ("-":mainFn:"--":args) = gatherVals srcs mainFn [] args
    gatherSrcs srcs mainFn ("--":args) = gatherVals srcs mainFn [] args
    gatherSrcs srcs mainFn (arg:args) = gatherSrcs (arg:srcs) mainFn args
    gatherVals srcs mainFn vals [] = (reverse srcs,mainFn,reverse vals)
    gatherVals srcs mainFn vals (arg:args) =
        gatherVals srcs mainFn (read arg:vals) args
    defaultMain srcFile =
        (takeWhile (not . (`elem` ".:=_")) . reverse . takeWhile (/= '/') . reverse) srcFile
