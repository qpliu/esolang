import Data.Bits(bit,testBit)
import Data.Char(chr,ord)
import System.Environment(getArgs)

import Compile(Defn,compile)
import Interp(interp)
import Parse(Definition,parseResolve)

main :: IO ()
main = do
    args <- getArgs
    parameters <- parseArgs args
    input <- fmap textToBits getContents
    either putStrLn (putStr . run input) parameters

parseArgs :: [String] -> IO (Either String (String,String,String))
parseArgs args =
    case args of
        [filename,mainFunc] -> do
            source <- readFile filename
            return (Right (filename,source,mainFunc))
        [filename] -> do
            source <- readFile filename
            return (Right (filename,source,getMainFunction filename))
        _ -> return (Left "Usage: flower SRC-FILE [MAIN-FUNCTION]")

run :: [Bool] -> (String,String,String) -> String
run input (filename,source,mainFunc) =
    either (unlines . map show) (maybe ("Unknown function:" ++ mainFunc ++ "\n") (bitsToStr . flip interp input) . flip ($) mainFunc . compile) (parseResolve filename source)

getMainFunction :: String -> String
getMainFunction = takeWhile (/= '.') . reverse . takeWhile (not . (`elem` "/\\")) . reverse

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
