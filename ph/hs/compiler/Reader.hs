module Reader(Value(Cons,Nil),foldValue) where

data Value = Nil | Cons Value Value
    deriving (Eq, Ord)

instance Read Value where
    readsPrec _ str =
        [(parensToValue $ map (== '(') $ filter (`elem` "()")
                        $ concat $ map (takeWhile (/= ';')) $ lines str,"")]
      where
        parensToValue :: [Bool] -> Value
        parensToValue (True:parens) = internalToValue parens const
        parensToValue _ = error "readValue"
        internalToValue :: [Bool] -> (Value -> [Bool] -> Value) -> Value
        internalToValue (True:parens) continuation =
            internalToValue parens
                (\ carValue cdrParens ->
                    internalToValue cdrParens
                        (\ cdrValue remainingParens ->
                            continuation (Cons carValue cdrValue)
                                         remainingParens))
        internalToValue (False:parens) continuation = continuation Nil parens
        internalToValue [] _ = error "readValue"

instance Show Value where
    show a = '(' : shows a ")" where
        shows Nil s = s
        shows (Cons a b) s = '(' : shows a (')' : shows b s)

foldValue :: (a -> Value -> a) -> a -> Value -> a
foldValue op a Nil = a
foldValue op a (Cons head tail) = foldValue op (op a head) tail
