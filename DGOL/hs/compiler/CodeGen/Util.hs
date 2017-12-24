module CodeGen.Util(
    intConst,nullConst,eq,uge,
    functionRef,globalRef,
    call
)
where

import Data.Word(Word32)

import LLVM.AST.Constant(Constant(Array,GlobalReference,Int,Null,Struct),constantType,integerBits,integerValue)
import LLVM.AST.Name(Name)
import LLVM.AST.Operand(Operand(ConstantOperand))
import qualified LLVM.AST.IntegerPredicate
import LLVM.AST.Type(Type(FunctionType),ptr,resultType,argumentTypes,isVarArg)
import qualified LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module(ModuleBuilder)
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

globalRef :: Name -> Type -> Operand
globalRef name typ = ConstantOperand (GlobalReference (ptr typ) name)

call :: MonadIRBuilder m => Operand -> [Operand] -> m Operand
call f args = LLVM.IRBuilder.Instruction.call f (map (flip (,) []) args)
