module ValueTrie(ValueTrie,empty,lookup,insert) where

import Prelude hiding (lookup,head,tail)
import Value(Value(Nil,Cons))

data ValueTrie a = ValueTrie (ValueTrie a) (ValueTrie a) (Maybe a) | Empty

head Empty = Empty
head (ValueTrie hd _ _) = hd

tail Empty = Empty
tail (ValueTrie _ tl _) = tl

value Empty = Nothing
value (ValueTrie _ _ val) = val

empty :: ValueTrie a
empty = Empty

insert :: Value -> a -> ValueTrie a -> ValueTrie a
insert key val trie =
    let insertValue trie = ValueTrie (head trie) (tail trie) (Just val)
    in  insert' key trie insertValue

insert' :: Value -> ValueTrie a -> (ValueTrie a -> ValueTrie a) -> ValueTrie a
insert' Nil trie insertValue = insertValue trie
insert' (Cons hd tl) trie insertValue =
    let insertTail trie' =
            ValueTrie (head trie')
                      (insert' tl (tail trie') insertValue)
                      (value trie')
    in  ValueTrie (insert' hd (head trie) insertTail) (tail trie) (value trie)

lookup :: Value -> ValueTrie a -> Maybe a
lookup = lookup' value

lookup' :: (ValueTrie a -> b) -> Value -> ValueTrie a -> b
lookup' value _ Empty = value Empty
lookup' value Nil trie = value trie
lookup' value (Cons hd tl) trie =
    case lookup' id hd (head trie) of
        Empty -> value Empty
        trie' -> lookup' value tl (tail trie')
