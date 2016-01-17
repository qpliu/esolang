module Value
    (Value(..),Data(..),
     newValue,reassignVar,copyValue,removeVars,valueBit,valueSetBit,valueField,
     refValue,unrefValue)
where

import Data.Map(fromList,member)

import LowLevel(Type(..))
import Memory(Memory,Ref,newRef,addRef,unref,deref,update)

data Data rtv = Data [Bool] [rtv]
data Value = Value Ref (Int,Int,Int,Int)
  deriving Show

instance Show (Data rtv) where
    show (Data bits _) = show bits

newValue :: (rtt -> rtv) -> Type rtt -> Memory (Data rtv) -> (Value,Memory (Data rtv))
newValue newRuntimeValue (Type bitSize rtt) mem = (value,newMem)
  where
    (newMem,ref) = newRef mem (Data (take bitSize (repeat False))
                              (map newRuntimeValue rtt))
    value = Value ref (0,0,bitSize,length rtt)

reassignVar :: String -> Value -> [(String,Value)] -> Memory (Data rtv)
                      -> ([(String,Value)],Memory (Data rtv))
reassignVar name newVal@(Value newRef _) scope mem = (newScope,newMem)
  where
    newMem = unref mem oldRef
    Just (Value oldRef _) = lookup name scope
    newScope = map updateVal scope
    updateVal var@(varName,_)
      | varName == name = (varName,newVal)
      | otherwise = var

copyValue :: Value -> Value -> Memory (Data rtv) -> Memory (Data rtv)
copyValue (Value srcRef (srcOffset,srcImportOffset,srcSize,srcImportSize))
          (Value destRef (destOffset,destImportOffset,destSize,destImportSize))
          mem
  | srcSize /= destSize || srcImportSize /= destImportSize = error "copyValue"
  | otherwise = update mem destRef copyData
  where
    (Data srcBits srcImports) = deref mem srcRef
    copyData (Data destBits destImports) =
        Data (take destOffset destBits ++
              take srcSize (drop srcOffset srcBits) ++
              drop (destOffset + srcSize) destBits)
             (take destImportOffset destImports ++
              take srcImportSize (drop srcImportOffset srcImports) ++
              drop (destImportOffset + srcImportSize) destImports)

removeVars :: [(String,a)] -> [(String,Value)] -> Memory (Data rtv)
                           -> ([(String,Value)],Memory (Data rtv))
removeVars vars scope mem = (reverse newScope,newMem)
  where
    varSet = fromList vars
    (newScope,newMem) = foldl checkVar ([],mem) scope
    checkVar (scope,mem) var@(varName,Value ref _)
      | member varName varSet = (scope,unref mem ref)
      | otherwise = (var:scope,mem)

valueBit :: Value -> Int -> Memory (Data rtv) -> Bool
valueBit (Value ref (offset,_,_,_)) index mem = bits !! (index + offset)
  where
    Data bits _ = deref mem ref

valueSetBit :: Value -> Int -> Bool -> Memory (Data rtv) -> Memory (Data rtv)
valueSetBit (Value ref (offset,_,_,_)) index bit mem = update mem ref setBit
  where
    setBit (Data bits imports) =
        (Data (take (index + offset) bits ++
               bit : drop (index + offset + 1) bits)
              imports)

valueField :: Value -> Int -> Int -> Type rtt -> Value
valueField (Value ref (offset,importOffset,_,_))
           fieldOffset fieldImportOffset (Type fieldSize fieldRtt) =
    Value ref (offset+fieldOffset,importOffset+fieldImportOffset,
               fieldSize,length fieldRtt)

refValue :: Value -> Memory (Data rtv) -> Memory (Data rtv)
refValue (Value ref _) mem = addRef mem ref

unrefValue :: Value -> Memory (Data rtv) -> Memory (Data rtv)
unrefValue (Value ref _) mem = unref mem ref
