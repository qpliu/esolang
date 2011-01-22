import Data.Bits(bit,testBit)
import Data.Char(chr,ord)
import Data.List((!!))
import Data.Map(Map,(!))
import System.Environment(getArgs)

import Parser
    (Defn(Defn),
     Expr(ExprLiteral,ExprParam,ExprFuncall),
     Param(ParamBound,ParamLiteral,ParamWild),
     arity,parse)

main :: IO ()
main = do
    args <- getArgs
    (sourceFiles,mainFunction,params) <- parseArgs args
    either (error . show)
           (putStr . bitsToStr . interp mainFunction (map textToBits params))
           (parse sourceFiles)

parseArgs :: [String] -> IO ([(FilePath,String)],String,[String])
parseArgs args = do
    (sourceFiles,mainFunction,args) <- readSources [] Nothing args
    stdin <- getContents
    params <- readParams stdin [] args
    return (reverse sourceFiles,
            maybe "main" id mainFunction,
            reverse (stdin:params))
  where
    readSources sourceFiles mainFunction [] =
        return (sourceFiles,mainFunction,[])
    readSources sourceFiles _ ("-":mainFunction:args) =
        return (sourceFiles,Just mainFunction,args)
    readSources sourceFiles mainFunction ("--":args) =
        return (sourceFiles,mainFunction,args)
    readSources sourceFiles mainFunction (filename:args) = do
        source <- readFile filename
        readSources ((filename,source):sourceFiles)
                    (maybe (Just (getMainFunction filename)) Just mainFunction)
                    args
    readParams stdin params [] = return params
    readParams stdin params ("-":args) = readParams stdin (stdin:params) args
    readParams stdin params (filename:args) = do
        contents <- readFile filename
        readParams stdin (contents:params) args

getMainFunction :: String -> String
getMainFunction filename =
    takeWhile (not . (`elem` ".01_="))
        $ reverse $ takeWhile (not . (`elem` "/\\")) $ reverse filename

interp :: String -> [[Bool]] -> Map String [Defn] -> [Bool]
interp mainFunction params fns =
    maybe (error ("No such function: " ++ mainFunction))
          (apply fns mainFunction . flip take (params ++ repeat []))
          (arity fns mainFunction)

textToBits :: String -> [Bool]
textToBits text = foldl (++) [] (map charToBits text) where
    charToBits char = map (testBit (ord char)) [7,6..0]

bitsToStr :: [Bool] -> String
bitsToStr [] = []
bitsToStr bits =
    let (byte,rest) = splitAt 8 bits
        bitToInt flag index = if flag then bit index else 0
        bitsToChar bs = chr (foldl (+) 0 (zipWith bitToInt bs [7,6..0]))
    in
        bitsToChar byte : bitsToStr rest

apply :: Map String [Defn] -> String -> [[Bool]] -> [Bool]
apply fns functionName args =
    let (exprs,params) = findMatch functionName (fns ! functionName) args
    in  concatMap (eval fns params) exprs

findMatch :: String -> [Defn] -> [[Bool]] -> ([Expr],[[Bool]])
findMatch functionName [] _ = error ("No matching pattern: " ++ functionName)
findMatch functionName (Defn params exprs:defns) args =
    maybe (findMatch functionName defns args)
          ((,) exprs) (matchArguments params args)

matchArguments :: [Param] -> [[Bool]] -> Maybe [[Bool]]
matchArguments params args = fmap reverse (matchArgs params args [])
  where
    matchArgs [] _ bindings = Just bindings
    matchArgs (ParamBound bits:params) (arg:args) bindings =
        maybe Nothing (matchArgs params args . (:bindings))
              (matchBits bits arg)
    matchArgs (ParamLiteral bits:params) (arg:args) bindings =
        maybe Nothing
              (\ b -> if null b
                        then matchArgs params args bindings else Nothing)
              (matchBits bits arg)
    matchArgs (ParamWild bits:params) (arg:args) bindings =
        maybe Nothing (const (matchArgs params args bindings))
              (matchBits bits arg)
    matchBits [] arg = Just arg
    matchBits _ [] = Nothing
    matchBits (b:bits) (a:arg) = if b == a then matchBits bits arg else Nothing

eval :: Map String [Defn] -> [[Bool]] -> Expr -> [Bool]
eval fns params (ExprLiteral bits) = bits
eval fns params (ExprParam index) = params !! index
eval fns params (ExprFuncall name args) =
    apply fns name (map (eval fns params) args)
