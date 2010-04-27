module Main(main) where

import System.Environment(getArgs)

import Interp(rootScope,eval)
import Value(Value(Nil),strToValue,valueToStr)

main :: IO ()
main = do
    args <- getArgs
    (toStr,code,arg) <- processArgs valueToStr args
    putStr $ toStr $ eval arg rootScope code

processArgs :: (Value -> String) -> [String] -> IO (Value -> String,Value,Value)
processArgs toStr [] = do
    src <- getContents
    return (toStr, read src, Nil)
processArgs _ ("-v":args) = processArgs show args
processArgs toStr (file:_) = do
    src <- readFile file
    input <- getContents
    return (toStr, read src, strToValue input)
