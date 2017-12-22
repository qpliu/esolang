{-# LANGUAGE OverloadedStrings #-}

module CodeGen.Runtime(
    runtimeModule,
    newNodeDecl
)
where

import LLVM.AST(Definition(GlobalDefinition))
import LLVM.AST.Constant(Constant(Array,GlobalReference,Int,Null,Struct),constantType,memberValues,integerBits,integerValue,isPacked,structName,memberType,memberValues)
import LLVM.AST.Global(initializer,name,type',globalVariableDefaults)
import LLVM.AST.Name(Name)
import LLVM.AST.Operand(Operand(ConstantOperand))
import LLVM.AST.Type(Type(StructureType),void,i1,i8,i64,elementTypes)
import qualified LLVM.AST.Type
import LLVM.IRBuilder.Module(ModuleBuilder,ParameterName(NoParameterName),emitDefn,extern,function)

import CodeGen.Types(
    nodeType,pNodeType,ppNodeType,pppNodeType,nodeTypedef,
    pageSize,pageType,pPageType,pageTypedef,
    frameType,pFrameType,frameTypedef,
    doEdgesIteratorType,pDoEdgesIteratorType,doEdgesIteratorTypedef)

newNodeName :: Name
newNodeName = "newNode"

newNodeDecl :: ModuleBuilder Operand
newNodeDecl = extern newNodeName [pFrameType] nodeType

newNodeImpl :: Operand -> ModuleBuilder Operand
newNodeImpl globalState = function newNodeName [(pFrameType,NoParameterName)] nodeType (\ [pFrame] -> do
    -- look for dead entries in the tables
    -- if none, garbage collect, then if last page is more than X% full,
    -- allocate a new last page
    undefined
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
                    structName = Nothing,
                    isPacked = False,
                    memberValues = [
                        Null { constantType = pPageType },
                        Array {
                            memberType = nodeType,
                            memberValues = replicate pageSize Struct {
                                structName = Nothing,
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
    return (ConstantOperand (GlobalReference globalStateType globalStateName))

runtimeModule :: ModuleBuilder ()
runtimeModule = do
    pageTypedef
    frameTypedef
    doEdgesIteratorTypedef
    globalState <- globalStateDef
    newNodeImpl globalState
    return ()
