module Scope(Scope,root,nested,get) where

data Scope a b = Root | Nested (Scope a b) (a -> Maybe b)

root :: (a -> Maybe b) -> Scope a b
root = Nested Root

nested :: Scope a b -> (a -> Maybe b) -> Scope a b
nested = Nested

get :: Show a => Scope a b -> a -> b
get scope@(Nested outer getValue) name =
    case getValue name of
        Just value -> value
        Nothing    -> get outer name
get Root name = error (show name)
