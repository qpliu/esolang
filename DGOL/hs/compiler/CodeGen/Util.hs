module CodeGen.Util(
    intConst,nullConst,eq,uge,
    functionRef,varArgsFunctionRef,varArgsExtern,globalRef,
    GlobalStrzTable(GlobalStrzTable),globalStrz,globalStrzAsI8Ptr,globalStrzDefs,
    call,
)
where

import Control.Monad(zipWithM_)
import Data.Char(ord)
import Data.String(fromString)
import Data.Word(Word32)

import LLVM.AST(Definition(GlobalDefinition))
import LLVM.AST.Constant(Constant(Array,GlobalReference,Int,Null,Struct),constantType,integerBits,integerValue,memberType,memberValues)
import LLVM.AST.Global(Parameter(Parameter),initializer,name,parameters,returnType,type',globalVariableDefaults,functionDefaults)
import LLVM.AST.Name(Name)
import LLVM.AST.Operand(Operand(ConstantOperand))
import qualified LLVM.AST.IntegerPredicate
import LLVM.AST.Type(Type(ArrayType,FunctionType),i8,ptr,resultType,argumentTypes,isVarArg,nArrayElements,elementType)
import qualified LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module(MonadModuleBuilder,emitDefn)
import LLVM.IRBuilder.Monad(MonadIRBuilder)

eq :: LLVM.AST.IntegerPredicate.IntegerPredicate
eq = LLVM.AST.IntegerPredicate.EQ

uge :: LLVM.AST.IntegerPredicate.IntegerPredicate
uge = LLVM.AST.IntegerPredicate.UGE

intConst :: Word32 -> Integer -> Operand
intConst bits value =
    ConstantOperand (Int { integerBits = bits, integerValue = value })

nullConst :: Type -> Operand
nullConst t = ConstantOperand (Null { constantType = t })

functionRef :: Name -> [Type] -> Type -> Operand
functionRef name argTypes resType =
    ConstantOperand (GlobalReference (ptr (FunctionType {
        resultType = resType,
        argumentTypes = argTypes,
        isVarArg = False
        })) name)

varArgsFunctionRef :: Name -> [Type] -> Type -> Operand
varArgsFunctionRef name argTypes resType =
    ConstantOperand (GlobalReference (ptr (FunctionType {
        resultType = resType,
        argumentTypes = argTypes,
        isVarArg = True
        })) name)

varArgsExtern :: MonadModuleBuilder m => Name -> [Type] -> Type -> m ()
varArgsExtern externName argTypes resType = do
    emitDefn $ GlobalDefinition functionDefaults {
        returnType = resType,
        name = externName,
        parameters = (map (\ t -> Parameter t (fromString "") []) argTypes,True)
        }

globalRef :: Name -> Type -> Operand
globalRef name typ = ConstantOperand (GlobalReference (ptr typ) name)

data GlobalStrzTable = GlobalStrzTable String [String]

globalStrzDefs :: MonadModuleBuilder m => GlobalStrzTable -> m ()
globalStrzDefs (GlobalStrzTable globalName strs) = zipWithM_ strzDef strs [1..]
  where
    strzDef str index =
        emitDefn $ GlobalDefinition globalVariableDefaults {
            name = fromString $ globalName ++ show index,
            type' = globalStrzType str,
            initializer = Just $ Array {
                memberType = i8,
                memberValues = map (Int 8 . fromIntegral . ord) str ++ [Int 8 0]
                }
            }

globalStrz :: GlobalStrzTable -> String -> Operand
globalStrz (GlobalStrzTable globalName strs) str =
    case lookup str (zip strs [1..]) of
      Just index ->
        ConstantOperand (GlobalReference (ptr $ globalStrzType str) (fromString $ globalName ++ show index))
      Nothing ->
        error $ "No " ++ show str ++ " in GlobalStrzTable " ++ globalName

globalStrzAsI8Ptr :: MonadIRBuilder m => GlobalStrzTable -> String -> m Operand
globalStrzAsI8Ptr table str =
    LLVM.IRBuilder.Instruction.gep (globalStrz table str) [intConst 32 0,intConst 32 0]

globalStrzType :: String -> Type
globalStrzType str = ArrayType {
    nArrayElements = fromIntegral $ 1 + length str,
    elementType = i8
    }

call :: MonadIRBuilder m => Operand -> [Operand] -> m Operand
call f args = LLVM.IRBuilder.Instruction.call f (map (flip (,) []) args)
