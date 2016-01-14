module Memory
    (Memory,Ref,newMemory,newRef,ref,unref,deref,update)
where

import Data.Map(Map,empty,insert,member,(!))
import qualified Data.Map as M

data Memory a = Memory Int (Map Ref (Int,a))
newtype Ref = Ref Int
  deriving (Eq, Ord)

newMemory :: Memory a
newMemory = Memory 0 empty

newRef :: Memory a -> a -> (Memory a,Ref)
newRef (Memory index memory) value =
    (Memory newIndex (insert (Ref newIndex) (1,value) memory),Ref newIndex)
  where
    newIndex = head (dropWhile (flip member memory . Ref) [index+1..])

ref :: Memory a -> Ref -> Memory a
ref (Memory index memory) ref = Memory index (M.update refEntry ref memory)
  where
    refEntry (n,a) = Just (n+1,a)

unref :: Memory a -> Ref -> Memory a
unref (Memory index memory) ref = Memory index (M.update unrefEntry ref memory)
  where
    unrefEntry (n,a) | n > 1 = Just (n-1,a) | otherwise = Nothing

deref :: Memory a -> Ref -> a
deref (Memory _ memory) ref = snd (memory ! ref)

update :: Memory a -> Ref -> (a -> a) -> Memory a
update (Memory index memory) ref f =
    Memory index (M.update (Just . fmap f) ref memory)
