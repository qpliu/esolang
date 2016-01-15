module InterpRuntime
    (InterpRuntimeType,InterpRuntimeFunc(..),InterpRuntimeValue,
     newRuntimeValue)
where

import Memory(Memory)
import Value(Value,Data)
import Runtime
    (RuntimeType(..),RuntimeFunc(..),
     Compile,SourcePos,compileError,
     AstType(..),AstFuncSig(..),astTypeName,astTypeSourcePos,astTypeErrorName)

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
    annotateFunc (AstFuncSig pos name@"getByte" [_] Nothing) =
        compileError pos ("Unknown import func '" ++ name ++ "'")
    annotateFunc (AstFuncSig pos name@"putByte" [_] Nothing) =
        compileError pos ("Unknown import func '" ++ name ++ "'")
    annotateFunc (AstFuncSig pos name _ _) =
        compileError pos ("Unknown import func '" ++ name ++ "'")

newRuntimeValue :: InterpRuntimeType -> InterpRuntimeValue
newRuntimeValue rtt = InterpRuntimeValue
