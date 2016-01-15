module Value
    (Value(..),Data(..),
     addVar,reassignVar,copyValue,removeVars,valueBit,valueField,
     refValue,unrefValue)
where

import Data.Map(fromList,member)

import LowLevel(Type(..))
import InterpRuntime(InterpRuntimeType,InterpRuntimeValue,newRuntimeValue)
import Memory(Memory,Ref,newRef,addRef,unref,deref,update)

data Data = Data [Bool] [InterpRuntimeValue]
data Value = Value Ref (Int,Int,Int,Int)

addVar :: String -> Type InterpRuntimeType -> Memory Data
                 -> ((String,Value),Memory Data)
addVar name (Type bitSize rtt) mem = ((name,val),newMem)
  where
    (newMem,ref) = newRef mem (Data (take bitSize (repeat False))
                              (map newRuntimeValue rtt))
    val = Value ref (0,0,bitSize,length rtt)

reassignVar :: String -> Value -> [(String,Value)] -> Memory Data
                      -> ([(String,Value)],Memory Data)
reassignVar name newVal@(Value newRef _) scope mem = (newScope,newMem)
  where
    newMem = unref (addRef mem newRef) oldRef
    Just (Value oldRef _) = lookup name scope
    newScope = map updateVal scope
    updateVal var@(varName,_)
      | varName == name = (varName,newVal)
      | otherwise = var

copyValue :: Value -> Value -> Memory Data -> Memory Data
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

removeVars :: [(String,a)] -> [(String,Value)] -> Memory Data
                           -> ([(String,Value)],Memory Data)
removeVars vars scope mem = (reverse newScope,newMem)
  where
    varSet = fromList vars
    (newScope,newMem) = foldl checkVar ([],mem) scope
    checkVar result@(scope,mem) (varName,Value ref _)
      | member varName varSet = (scope,unref mem ref)
      | otherwise = result

valueBit :: Value -> Int -> Memory Data -> Bool
valueBit (Value ref (offset,_,_,_)) index mem = bits !! index
  where
    Data bits _ = deref mem ref

valueField :: Value -> Int -> Int -> Type InterpRuntimeType -> Value
valueField (Value ref (offset,importOffset,_,_))
           fieldOffset fieldImportOffset (Type fieldSize fieldRtt) =
    Value ref (offset+fieldOffset,importOffset+fieldImportOffset,
               fieldSize,length fieldRtt)

refValue :: Memory Data -> Value -> Memory Data
refValue mem (Value ref _) = addRef mem ref

unrefValue :: Memory Data -> Value -> Memory Data
unrefValue mem (Value ref _) = unref mem ref
