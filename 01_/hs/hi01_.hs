module Main(main) where

import Data.Bits(bit,testBit)
import Data.Char(chr,ord)
import Data.List(splitAt)
import System.Environment(getArgs)

import Interp(apply,arity)
import ParseBodies(parseBodies)
import ParseSigs(parseSigs)
import Tokenizer(tokenize)

main :: IO ()

main = do
    stdinText <- getContents
    interpArgs <- getArgs
    let (files,fn,argFiles) = parseArgs interpArgs []
        stdinBits = textToBits stdinText
    sigs <- readSources files []
    args <- readArgs stdinBits argFiles []
    let defs = parseBodies sigs
    let nils = []:nils
    let result = apply defs fn (take (arity defs fn) (args ++ stdinBits:nils))
    putStr (bitsToStr result)

parseArgs ("-":fn:args) files = (reverse files,fn,args)
parseArgs ["-"] files = (reverse files,defaultFn files,[])
parseArgs (file:args) files = parseArgs args (file:files)
parseArgs [] files = (reverse files,defaultFn files,[])

defaultFn (fn:_) =
    (fst . (break (=='.')) . reverse . fst . (break (=='/')) . reverse) fn

readSources [] sigs = return sigs
readSources (file:files) sigs = do
    text <- readFile file
    let s = sigs ++ ((parseSigs . tokenize) text)
    readSources files s

readArgs _ [] args = return (reverse args)
readArgs stdinBits ("-":files) args =
    readArgs stdinBits files (stdinBits:args)
readArgs stdinBits (file:files) args = do
    text <- readFile file
    readArgs stdinBits files (textToBits text:args)

textToBits text = foldl (++) [] (map charToBits text) where
    charToBits char = map (testBit (ord char)) [7,6..0]

bitsToStr [] = []
bitsToStr bits =
    let (byte,rest) = splitAt 8 bits
        bitToInt flag index = if flag then bit index else 0
        bitsToChar bs = chr (foldl (+) 0 (zipWith bitToInt bs [7,6..0]))
    in
        bitsToChar byte : bitsToStr rest
