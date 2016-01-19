module LLVMGen
    (CodeGen,Label,Temp,
     newTemp,newLabel,
     writeCode,writeRefCountType,writeOffsetType,
     writeTemp,writeLabel,writeLabelRef,writeName,
     gen)
where

import Control.Monad.State(State,get,put,execState)
import Data.Char(isAlphaNum,isAscii,ord)

data CodeGenState = CodeGenState (Temp,Label) (String,String) [String]

type CodeGen a = State CodeGenState a
newtype Label = Label Int
newtype Temp = Temp Int

gen :: Int -> Int -> CodeGen () -> String
gen maxRefCount maxOffset codeGen = concat (reverse code)
  where
    CodeGenState _ _ code =
        execState codeGen (CodeGenState (Temp 0,Label 0) auxTypes [])
    auxTypes = (getAuxType maxRefCount,getAuxType maxOffset)
    getAuxType maxVal | maxVal < 256 = "i8"
                      | maxVal < 65536 = "i16"
                      | otherwise = "i32"

newTemp :: CodeGen Temp
newTemp = do
    CodeGenState (Temp t,label) auxTypes code <- get
    put (CodeGenState (Temp (t+1),label) auxTypes code)
    return (Temp t)

newLabel :: CodeGen Label
newLabel = do
    CodeGenState (temp,Label l) auxTypes code <- get
    put (CodeGenState (temp,Label (l+1)) auxTypes code)
    return (Label l)

writeCode :: String -> CodeGen ()
writeCode newcode = do
    CodeGenState counters auxTypes code <- get
    put (CodeGenState counters auxTypes (newcode:code))

writeRefCountType :: CodeGen ()
writeRefCountType = do
    CodeGenState counters auxTypes@(refCountType,_) code <- get
    put (CodeGenState counters auxTypes (refCountType:code))

writeOffsetType :: CodeGen ()
writeOffsetType = do
    CodeGenState counters auxTypes@(_,offsetType) code <- get
    put (CodeGenState counters auxTypes (offsetType:code))

writeTemp :: Temp -> CodeGen ()
writeTemp (Temp t) = writeCode ("%" ++ show t)

writeLabel :: Label -> CodeGen ()
writeLabel (Label l) = writeCode ("l" ++ show l ++ ":")

writeLabelRef :: Label -> CodeGen ()
writeLabelRef (Label l) = writeCode ("%l" ++ show l)

writeName :: String -> CodeGen ()
writeName "main" = writeCode "main"
writeName name = writeCode ("_" ++ concatMap escape name)
  where
    escape ch | isAscii ch && isAlphaNum ch = [ch]
              | otherwise = "_" ++ show (ord ch) ++ "_"
