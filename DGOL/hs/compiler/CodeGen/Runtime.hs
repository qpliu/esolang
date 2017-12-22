{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module CodeGen.Runtime(
    runtimeModule,
    newNodeDecl
)
where

import Data.Word(Word32)

import LLVM.AST(Definition(GlobalDefinition),Module)
import LLVM.AST.Constant(Constant(Array,GlobalReference,Int,Null,Struct),constantType,memberValues,integerBits,integerValue,isPacked,structName,memberType,memberValues)
import LLVM.AST.Global(initializer,name,type',globalVariableDefaults)
import LLVM.AST.Instruction(Instruction(GetElementPtr),inBounds,address,indices,metadata)
import LLVM.AST.IntegerPredicate(IntegerPredicate(UGE))
import qualified LLVM.AST.IntegerPredicate
import LLVM.AST.Name(Name)
import LLVM.AST.Operand(Operand(ConstantOperand))
import LLVM.AST.Type(Type(StructureType),void,i1,i8,i64,elementTypes,ptr)
import qualified LLVM.AST.Type
import LLVM.IRBuilder.Instruction(add,br,condBr,gep,icmp,load,phi,ret)
import LLVM.IRBuilder.Module(ModuleBuilder,ParameterName(NoParameterName),emitDefn,extern,function,buildModule)
import LLVM.IRBuilder.Monad(emitInstr,block)

import CodeGen.Types(
    nodeTypeName,nodeType,pNodeType,ppNodeType,pppNodeType,nodeTypedef,
    pageSize,pageTypeName,pageType,pPageType,pageTypedef,
    frameTypeName,frameType,pFrameType,frameTypedef,
    doEdgesIteratorTypeName,doEdgesIteratorType,pDoEdgesIteratorType,doEdgesIteratorTypedef)

newNodeName :: Name
newNodeName = "newNode"

newNodeDecl :: ModuleBuilder Operand
newNodeDecl = extern newNodeName [pFrameType] nodeType

newNodeImpl :: Operand -> ModuleBuilder Operand
newNodeImpl globalState = function newNodeName [(pFrameType,NoParameterName)] pNodeType (\ [pFrame] -> mdo
    -- look for dead entries in the tables
    -- if none, garbage collect, then if last page is more than X% full,
    -- allocate a new last page

    entry <- block
    initialPage <- gep globalState [intConst 32 0, intConst 32 1]
    br pageLoop

    pageLoop <- block
    page <- phi [
        (initialPage,entry),
        (page,pageLoopNextIndex),
        (nextPage,pageLoopNextPage)
        ]
    index <- phi [
        (intConst 32 0,entry),
        (nextIndex,pageLoopNextIndex),
        (intConst 32 0,pageLoopNextPage)
        ]
    -- if page.nodes[index].alive is 0,
    --    set page.nodes[index].alive to 1
    --    if page.nodes[index].edges is not null
    --        clear page.nodes[index].edges
    --    return &page.nodes[index]
    -- else branch to pageLoopNextIndex
    br pageLoopNextIndex

    pageLoopNextIndex <- block
    nextIndex <- add index (intConst 32 1)
    rangeCheck <- icmp UGE nextIndex (intConst 32 pageSize)
    condBr rangeCheck pageLoopNextPage pageLoop

    pageLoopNextPage <- block
    nextPagePtr <- gep page [intConst 32 0, intConst 32 0]
    nextPage <- load nextPagePtr 0
    nullCheck <- icmp LLVM.AST.IntegerPredicate.EQ nextPage (nullConst pPageType)
    condBr nullCheck startGC pageLoop

    startGC <- block
    ret (nullConst nodeType)
    )

globalStateName :: Name
globalStateName = "globalState"

globalStateType :: Type
globalStateType = StructureType {
    LLVM.AST.Type.isPacked = False,
    elementTypes = [
        i8, -- gc mark
        pageType -- root page
        ]
    }

globalStateDef :: ModuleBuilder Operand
globalStateDef = do
    emitDefn (GlobalDefinition globalVariableDefaults {
        name = globalStateName,
        type' = globalStateType,
        initializer = Just (Struct { -- zeroinitializer would have been nice
            structName = Nothing,
            isPacked = False,
            memberValues = [
                Int { integerBits = 8, integerValue = 0 },
                Struct {
                    structName = Just pageTypeName,
                    isPacked = False,
                    memberValues = [
                        Null { constantType = pPageType },
                        Array {
                            memberType = nodeType,
                            memberValues = replicate pageSize Struct {
                                structName = Just nodeTypeName,
                                isPacked = False,
                                memberValues = [
                                    Int { integerBits = 8, integerValue = 0 },
                                    Int { integerBits = 1, integerValue = 0 },
                                    Int { integerBits = 64, integerValue = 0 },
                                    Null { constantType = ppNodeType }
                                    ]
                                }
                            }
                        ]
                    }
                ]
            })
        })
    return (ConstantOperand (GlobalReference (ptr globalStateType) globalStateName))

intConst :: Word32 -> Integer -> Operand
intConst bits value =
    ConstantOperand (Int { integerBits = bits, integerValue = value })

nullConst :: Type -> Operand
nullConst t = ConstantOperand (Null { constantType = t })

runtimeModule :: Module
runtimeModule = buildModule "DGOL.runtime" $ do
    nodeTypedef
    pageTypedef
    frameTypedef
    doEdgesIteratorTypedef
    globalState <- globalStateDef
    newNodeImpl globalState
