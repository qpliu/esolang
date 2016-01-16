module InterpRuntime
    (InterpRuntimeType,InterpRuntimeFunc(..),InterpRuntimeValue,
     newRuntimeValue)
where

import Data.Bits(testBit,setBit)
import Data.Char(chr,ord)
import System.IO(isEOF)

import Memory(Memory)
import Value(Value,Data,unrefValue,valueSetBit,valueBit)
import Runtime
    (RuntimeType(..),RuntimeFunc(..),
     Compile,SourcePos,compileError,
     AstType(..),AstFuncSig(..),
     astTypeName,astTypeSourcePos,astTypeIsImport,astTypeErrorName,astTypeSize)

type Mem = Memory (Data InterpRuntimeValue)
data InterpRuntimeType = InterpRuntimeType
newtype InterpRuntimeFunc =
    InterpRuntimeFunc (Mem -> [Value] -> IO (Mem, Maybe Value))
data InterpRuntimeValue = InterpRuntimeValue

instance RuntimeType InterpRuntimeType where
    annotateType astType
      | astTypeName astType == "stack" =
            compileError (astTypeSourcePos astType)
                         ("Unknown import " ++ astTypeErrorName astType)
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
            compileError pos ("Unknown import func '" ++ name ++ "'")
      | otherwise =
            compileError pos ("Unknown import func '" ++ name ++ "'")
    annotateFunc (AstFuncSig pos name@"popStack" [(_,stack)] (Just bit))
      | astTypeIsImport stack && astTypeName stack == "stack" =
            compileError pos ("Unknown import func '" ++ name ++ "'")
      | otherwise =
            compileError pos ("Unknown import func '" ++ name ++ "'")
    annotateFunc (AstFuncSig pos name@"isEmptyStack" [(_,stack)] (Just bit))
      | astTypeIsImport stack && astTypeName stack == "stack" =
            compileError pos ("Unknown import func '" ++ name ++ "'")
      | otherwise =
            compileError pos ("Unknown import func '" ++ name ++ "'")
    annotateFunc (AstFuncSig pos name _ _) =
        compileError pos ("Unknown import func '" ++ name ++ "'")

newRuntimeValue :: InterpRuntimeType -> InterpRuntimeValue
newRuntimeValue rtt = InterpRuntimeValue

getByte :: AstType -> Mem -> [Value] -> IO (Mem,Maybe Value)
getByte astType = func
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

putByte :: AstType -> Mem -> [Value] -> IO (Mem,Maybe Value)
putByte astType = func
  where
    getValue value mem byte index
      | valueBit value index mem = setBit byte index
      | otherwise = byte
    func mem [value] = do
        putChar (chr (foldl (getValue value mem) 0
                            [0 .. min (astTypeSize astType - 1) 7]))
        return (unrefValue value mem,Nothing)
