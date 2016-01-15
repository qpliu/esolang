module InterpRuntime
    (InterpRuntimeType,InterpRuntimeFunc,InterpRuntimeValue,
     newRuntimeValue)
where

import Runtime
    (RuntimeType(..),RuntimeFunc(..),
     Compile,SourcePos,compileError,
     AstType(..),AstFuncSig(..),astTypeName,astTypeSourcePos,astTypeErrorName)

data InterpRuntimeType = InterpRuntimeType
data InterpRuntimeFunc = InterpRuntimeFunc
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
