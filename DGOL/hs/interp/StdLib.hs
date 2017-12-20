module StdLib(stdlib)
where

import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified System.IO as IO
import qualified Scope

stdlib :: [(String,Map.Map String (Scope.Scope -> [Scope.Var] -> IO Scope.Scope))]
stdlib = [("IO",Map.fromList iolib)]

iolib :: [(String,Scope.Scope -> [Scope.Var] -> IO Scope.Scope)]
iolib = [("READBYTE",iolib_readbyte),("WRITEBYTE",iolib_writebyte)]

iolib_readbyte :: Scope.Scope -> [Scope.Var] -> IO Scope.Scope
iolib_readbyte scope vars = do
    eof <- IO.isEOF
    if eof
      then return (setBit scope (1,True))
      else do
        byte <- fmap Char.ord getChar
        return (foldl setBit scope ((1,False):zip [2..9] (bits byte)))
  where
    setBit scope (index,set)
      | length vars <= index = scope
      | set = Scope.addEdge scope (vars !! 0) (vars !! index)
      | otherwise = Scope.removeEdge scope (vars !! 0) (vars !! index)
    bits byte = (mod byte 2 == 1) : bits (div byte 2)

iolib_writebyte :: Scope.Scope -> [Scope.Var] -> IO Scope.Scope
iolib_writebyte scope vars = do
    putChar (Char.chr (bit1 + bit2 + bit4 + bit8 + bit10 + bit20 + bit40 + bit80))
    return scope
  where
    bit1 | length vars >= 2 && Scope.hasEdge scope (vars !! 0) (vars !! 1) = 1
         | otherwise = 0
    bit2 | length vars >= 3 && Scope.hasEdge scope (vars !! 0) (vars !! 2) = 2
         | otherwise = 0
    bit4 | length vars >= 4 && Scope.hasEdge scope (vars !! 0) (vars !! 3) = 4
         | otherwise = 0
    bit8 | length vars >= 5 && Scope.hasEdge scope (vars !! 0) (vars !! 4) = 8
         | otherwise = 0
    bit10 | length vars >= 6 && Scope.hasEdge scope (vars !! 0) (vars !! 5) = 16
          | otherwise = 0
    bit20 | length vars >= 7 && Scope.hasEdge scope (vars !! 0) (vars !! 6) = 32
          | otherwise = 0
    bit40 | length vars >= 8 && Scope.hasEdge scope (vars !! 0) (vars !! 7) = 64
          | otherwise = 0
    bit80 | length vars >= 9 && Scope.hasEdge scope (vars !! 0) (vars !! 8) = 128
          | otherwise = 0
