module Value
    (Value,empty,union,intersect,diff,fromList)
where

import Data.Map(Map)
import qualified Data.Map as Map

data Value = Value (Map Value Int)
  deriving (Eq,Ord,Show)

empty :: Value
empty = Value Map.empty

union :: Value -> Value -> Value
union (Value a) (Value b) = Value (Map.unionWith max a b)

intersect :: Value -> Value -> Value
intersect (Value a) (Value b) = Value (Map.intersectionWith min a b)

diff :: Value -> Value -> Value
diff (Value a) (Value b) = Value (Map.mergeWithKey difference id id a b)
  where
    difference _ a b | a == b = Nothing | otherwise = Just (abs (a - b))

fromList :: [(Int,Value)] -> Value
fromList values = foldl collect empty values
  where
    collect (Value v) (n,elt) = Value (Map.alter (add n) elt v)
    add n count = Just (maybe n (+ n) count)
