module Value(Value(Nil,Cons),isNil,car,cdr,strToValue,valueToStr) where

import Data.Bits(bit,testBit)
import Data.Char(chr,ord)

data Value = Nil | Cons Value Value
    deriving (Eq, Ord)

instance Show Value where
    show a = '(' : shows a ")" where
        shows Nil s = s
        shows (Cons a b) s = '(' : shows a (')' : shows b s)

isNil :: Value -> Bool
isNil Nil = True
isNil _ = False

car :: Value -> Value
car (Cons a _) = a
car Nil = Nil

cdr :: Value -> Value
cdr (Cons _ b) = b
cdr Nil = Nil

instance Read Value where
    readsPrec p s@('(':_) = [readNext s]
    readsPrec p (_:s) = readsPrec p s
    readsPrec p [] = []

readNext :: String -> (Value,String)
readNext ('(':s) = readNext' s
  where
    readNext' ('(':s) =
        let (car,s') = readNext' s
            (cdr,s'') = readNext' s'
        in  (Cons car cdr,s'')
    readNext' (')':s) = (Nil,s)
    readNext' (_:s) = readNext' s
    readNext' [] = error "unmatched ("

readNext (')':_) = error "unmatched )"
readNext (_:s) = readNext s
readNext [] = error "unmatched ("

strToValue :: String -> Value
strToValue [] = Cons Nil Nil
strToValue (c:cs) = bitsToValue [7,6..0] c
  where
    bitsToValue [] _ = strToValue cs
    bitsToValue (b:bs) byte =
        (if testBit (ord c) b then Cons else flip Cons)
            (bitsToValue bs byte) Nil

valueToStr :: Value -> String
valueToStr value = valueToBits [7,6..0] 0 value
  where
    valueToBits [] byte rest = chr byte : valueToStr rest
    valueToBits _ _ Nil = []
    valueToBits (_:bs) byte (Cons Nil rest) = valueToBits bs byte rest
    valueToBits (b:bs) byte (Cons rest _) = valueToBits bs (byte + bit b) rest
