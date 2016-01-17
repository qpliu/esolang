module InterpRuntime
    (InterpRuntimeType,InterpRuntimeFunc(..),InterpRuntimeValue,
     newRuntimeValue)
where

import Data.Bits(testBit,setBit)
import Data.Char(chr,ord)
import System.IO(isEOF)

import Memory(Memory)
import Value
    (Value(..),Data,
     unrefValue,valueSetBit,valueBit,runtimeValue,updateRuntimeValue)
import Runtime
    (RuntimeType(..),RuntimeFunc(..),
     Compile,SourcePos,compileError,
     AstType(..),AstFuncSig(..),
     astTypeName,astTypeSourcePos,astTypeIsImport,astTypeErrorName,astTypeSize)

type Mem = Memory (Data InterpRuntimeValue)
data InterpRuntimeType = IRTTStack
newtype InterpRuntimeFunc =
    InterpRuntimeFunc (Maybe (Mem -> (Value,Mem)) -> Mem -> [Value]
                                                  -> IO (Mem,Maybe Value))
data InterpRuntimeValue = IRTVStack [Bool]

instance RuntimeType InterpRuntimeType where
    annotateType astType
      | astTypeName astType == "stack" = return IRTTStack
      | otherwise =
            compileError (astTypeSourcePos astType)
                         ("Unknown import " ++ astTypeErrorName astType)

instance RuntimeFunc InterpRuntimeFunc where
    annotateFunc (AstFuncSig pos name@"getByte" [(_,astType)] Nothing) =
        return (InterpRuntimeFunc (getByte astType))
    annotateFunc (AstFuncSig pos name@"putByte" [(_,astType)] Nothing) =
        return (InterpRuntimeFunc (putByte astType))
    annotateFunc (AstFuncSig pos name@"pushStack" [(_,stack),(_,bit)] Nothing)
      | astTypeIsImport stack && astTypeName stack == "stack" =
            return (InterpRuntimeFunc (pushStack bit))
    annotateFunc (AstFuncSig pos name@"popStack" [(_,stack)] (Just bit))
      | astTypeIsImport stack && astTypeName stack == "stack" =
            return (InterpRuntimeFunc (popStack bit))
    annotateFunc (AstFuncSig pos name@"isEmptyStack" [(_,stack)] (Just bit))
      | astTypeIsImport stack && astTypeName stack == "stack" =
            return (InterpRuntimeFunc (isEmptyStack bit))
    annotateFunc (AstFuncSig pos name _ _) =
        compileError pos ("Unknown import func '" ++ name ++ "'")

newRuntimeValue :: InterpRuntimeType -> InterpRuntimeValue
newRuntimeValue IRTTStack = IRTVStack []

getByte :: AstType -> Maybe (Mem -> (Value,Mem)) -> Mem -> [Value]
                   -> IO (Mem,Maybe Value)
getByte astType _ = func
  where
    valueSize = astTypeSize astType
    setValue ch value mem index =
        valueSetBit value index (testBit (ord ch) index) mem
    func mem [value] = do
        eof <- isEOF
        let mem1 = if valueSize < 8 then mem else valueSetBit value 8 eof mem
        mem2 <- if eof
                    then return mem1
                    else do
                        ch <- getChar
                        return (foldl (setValue ch value) mem1
                                      [0..min 7 (valueSize - 1)])
        return (unrefValue value mem2,Nothing)

putByte :: AstType -> Maybe (Mem -> (Value,Mem)) -> Mem -> [Value]
                   -> IO (Mem,Maybe Value)
putByte astType _ = func
  where
    getValue value mem byte index
      | valueBit value index mem = setBit byte index
      | otherwise = byte
    func mem [value] = do
        putChar (chr (foldl (getValue value mem) 0
                            [0 .. min (astTypeSize astType - 1) 7]))
        return (unrefValue value mem,Nothing)

pushStack :: AstType -> Maybe (Mem -> (Value,Mem)) -> Mem -> [Value]
                     -> IO (Mem,Maybe Value)
pushStack bitType _ mem [stackVal,bitVal] =
    return ((unrefValue stackVal . unrefValue bitVal) newMem,Nothing)
  where
    bit = valueBit bitVal 0 mem
    newMem
      | astTypeSize bitType == 0 = mem
      | otherwise =
            updateRuntimeValue stackVal
                               (\ (IRTVStack bits) -> IRTVStack (bit:bits)) mem

popStack :: AstType -> Maybe (Mem -> (Value,Mem)) -> Mem -> [Value]
                    -> IO (Mem,Maybe Value)
popStack bitType (Just makeRetVal) mem [stackVal] =
    return (unrefValue stackVal mem3,Just retVal)
  where
    IRTVStack bits = runtimeValue stackVal mem
    (retVal,mem1) = makeRetVal mem
    mem2 | astTypeSize bitType == 0 = mem1
         | otherwise = valueSetBit retVal 0 (head bits) mem1
    mem3 = updateRuntimeValue stackVal (const (IRTVStack (tail bits))) mem2
                

isEmptyStack :: AstType -> Maybe (Mem -> (Value,Mem)) -> Mem -> [Value]
                        -> IO (Mem,Maybe Value)
isEmptyStack bitType (Just makeRetVal) mem [stackVal] =
    return (unrefValue stackVal mem2,Just retVal)
  where
    IRTVStack bits = runtimeValue stackVal mem
    (retVal,mem1) = makeRetVal mem
    mem2 | astTypeSize bitType == 0 = mem1
         | otherwise = valueSetBit retVal 0 (null bits) mem1
