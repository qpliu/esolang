module Encoding
    (fromString,toString)
where

import Data.Bits(testBit)
import Data.Char(chr,ord)

import Value(Value,empty,fromList,toList)

fromBits :: [Bool] -> Value
fromBits [] = empty
fromBits (True:bits) = fromList [(1,empty),(1,fromBits bits)]
fromBits (False:bits) = fromList [(1,fromBits bits)]

fromString :: String -> Value
fromString s = fromBits (concatMap charToBits s)
  where
    charToBits c = map (testBit (ord c)) [7,6..0]

is1bit :: Value -> Bool
is1bit v = hasEmpty (toList v)
  where
    hasEmpty [(2,v)] = v == empty
    hasEmpty [(1,v1),(1,v2)] = v1 == empty || v2 == empty
    hasEmpty _ = False

next :: Value -> Maybe Value
next v = getNext (toList v)
  where
    getNext [(1,v)] = Just v
    getNext [(1,v1),(1,v2)]
      | v1 == empty = Just v2 | v2 == empty = Just v1 | otherwise = Nothing
    getNext [(2,v)] | v == empty = Just v | otherwise = Nothing
    getNext _ = Nothing

toBits :: Value -> [Bool]
toBits v = maybe [] ((is1bit v :) . toBits) (next v)

toString :: Value -> String
toString v = bitsToStr (toBits v)
  where
    bitsToStr (b80:b40:b20:b10:b8:b4:b2:b1:bits) =
        chr ((if b80 then 128 else 0) + (if b40 then 64 else 0) +
             (if b20 then 32 else 0) +  (if b10 then 16 else 0) +
             (if b8 then 8 else 0) +    (if b4 then 4 else 0) +
             (if b2 then 2 else 0) +    (if b1 then 1 else 0)) : bitsToStr bits
    bitsToStr _ = []
