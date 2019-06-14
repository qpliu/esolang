module Tree(Tree,cond,cons,left,right,value,visualize)
where

import Data.List(intercalate)

data Tree = Tree Bool Tree Tree

cons :: Bool -> Tree -> Tree -> Tree
cons = Tree

left :: Tree -> Tree
left (Tree _ l _) = l

right :: Tree -> Tree
right (Tree _ _ r) = r

cond :: Tree -> Tree -> Tree -> Tree
cond (Tree v _ _) f t | v = t | otherwise = f

value :: Tree -> Bool
value (Tree v _ _) = v

visualize :: Tree -> Int -> [String]
visualize (Tree v l r) depth
  | depth < 0 = []
  | depth == 0 = [value]
  | otherwise = (indent ++ value ++ indent) : zipWith join (visualize l (depth-1)) (visualize r (depth-1))
  where
    value = if v then "1" else "0"
    indent = replicate (2^depth-1) ' '
    join l r = l ++ " " ++ r
