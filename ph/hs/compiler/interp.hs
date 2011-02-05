import Data.Bits(bit,testBit)
import Data.Char(chr,ord)
import qualified Data.Map as Map
import System.Environment(getArgs)

import Flattener(Expr(Arg,Quote,CarFn,CdrFn,ConsFn,If,Call),flatten)
import Parser(parse)
import Reader(Value(Cons,Nil),readValue)

main :: IO ()
main = do
    args <- getArgs
    (fns,expr,input,outputValue) <- handleArgs args
    outputValue (eval fns expr input)

handleArgs :: [String] -> IO (Map.Map Int Expr,Expr,Value,Value -> IO ())
handleArgs [] = runParameters getContents (return []) outputBits
handleArgs ["-v"] = runParameters getContents (return []) print
handleArgs ("-v":file:_) = runParameters (readFile file) getContents print
handleArgs (file:_) = runParameters (readFile file) getContents outputBits

runParameters :: IO String -> IO String -> (Value -> IO ())
                           -> IO (Map.Map Int Expr,Expr,Value,Value -> IO ())
runParameters getSource getInput outputValue = do
    (fns,expr) <- fmap (flatten . parse . readValue) getSource
    input <- fmap (bitsToValue . stringToBits) getInput
    return (Map.map snd fns,expr,input,outputValue)

outputBits :: Value -> IO ()
outputBits value = putStr (bitsToString (valueToBits value))

stringToBits :: String -> [Bool]
stringToBits str = concatMap (flip map [7,6..0] . testBit . ord) str

bitsToString :: [Bool] -> String
bitsToString [] = ""
bitsToString bits = bitsToChar (take 8 bits) : bitsToString (drop 8 bits)
  where
    bitsToChar bits =
        chr (sum (zipWith (\ b f -> if f then bit b else 0) [7,6..0] bits))

bitsToValue :: [Bool] -> Value
bitsToValue [] = Cons Nil Nil
bitsToValue (bit:bits) =
    (if bit then Cons else flip Cons) (bitsToValue bits) Nil

valueToBits :: Value -> [Bool]
valueToBits Nil = []
valueToBits (Cons Nil Nil) = []
valueToBits (Cons Nil rest) = False : valueToBits rest
valueToBits (Cons rest _) = True : valueToBits rest

eval :: Map.Map Int Expr -> Expr -> Value -> Value
eval fns Arg arg = arg
eval fns (Quote value) arg = value
eval fns (CarFn expr) arg = car (eval fns expr arg)
  where
    car Nil = Nil
    car (Cons head _) = head
eval fns (CdrFn expr) arg = cdr (eval fns expr arg)
  where
    cdr Nil = Nil
    cdr (Cons _ tail) = tail
eval fns (ConsFn expr1 expr2) arg =
    Cons (eval fns expr1 arg) (eval fns expr2 arg)
eval fns (If cond ifTrue ifFalse) arg = evalIf (eval fns cond arg)
  where
    evalIf Nil = eval fns ifFalse arg
    evalIf _ = eval fns ifTrue arg
eval fns (Call fn argExpr) arg =
    eval fns ((Map.!) fns fn) (eval fns argExpr arg)
