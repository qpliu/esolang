module Encoding(fromBits,fromString,toBits,toString)
where

import Data.Bits(testBit)
import Data.Char(chr,ord)

import Tree(Tree,cons,left,value)

zeros :: Tree
zeros = cons False zeros zeros

fromBits :: [Bool] -> Tree
fromBits [] = zeros
fromBits (bit:bits) = cons True (cons bit (fromBits bits) zeros) zeros

toBits :: Tree -> [Bool]
toBits t
  | not (value t) = []
  | otherwise = value (left t) : toBits (left (left t))

fromString :: String -> Tree
fromString str = fromBits (concatMap charToBits str)
  where
    charToBits c = map (testBit (ord c)) [0..7]

toString :: Tree -> String
toString t = bitsToStr (toBits t)
  where
    bitsToStr (b1:b2:b4:b8:b10:b20:b40:b80:bits) =
        chr ((if b80 then 128 else 0) + (if b40 then 64 else 0) +
             (if b20 then 32 else 0) +  (if b10 then 16 else 0) +
             (if b8 then 8 else 0) +    (if b4 then 4 else 0) +
             (if b2 then 2 else 0) +    (if b1 then 1 else 0)) : bitsToStr bits
    bitsToStr _ = []
