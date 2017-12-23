{-# LANGUAGE OverloadedStrings #-}

module CodeGen.Types(
    nodeTypeName,nodeType,pNodeType,ppNodeType,pppNodeType,nodeTypedef,
    pageSize,newPageThreshold,
    pageTypeName,pageType,pPageType,pageTypedef,
    frameTypeName,frameType,pFrameType,frameTypedef,
    doEdgesIteratorTypeName,doEdgesIteratorType,pDoEdgesIteratorType,doEdgesIteratorTypedef
)
where

import LLVM.AST.Name(Name)
import LLVM.AST.Type(Type(ArrayType,NamedTypeReference,StructureType),i1,i8,i32,ptr,isPacked,elementTypes,nArrayElements,elementType)
import LLVM.IRBuilder.Module(ModuleBuilder,typedef)

nodeTypeName :: Name
nodeTypeName = "node"

nodeType :: Type
nodeType = NamedTypeReference nodeTypeName

pNodeType :: Type
pNodeType = ptr nodeType

ppNodeType :: Type
ppNodeType = ptr pNodeType

pppNodeType :: Type
pppNodeType = ptr ppNodeType

nodeTypedef :: ModuleBuilder ()
nodeTypedef = typedef nodeTypeName (Just (StructureType {
    isPacked = False,
    elementTypes = [
        i8, -- gc mark
        i1, -- live flag
        i32, -- size of allocated array of edges
        ppNodeType -- array of edges
        ]
    }))

pageSize :: Num a => a
pageSize = 256

newPageThreshold :: Num a => a
newPageThreshold = 240

pageTypeName :: Name
pageTypeName = "page"

pageType :: Type
pageType = NamedTypeReference pageTypeName

pPageType :: Type
pPageType = ptr pageType

pageTypedef :: ModuleBuilder ()
pageTypedef = typedef pageTypeName (Just (StructureType {
    isPacked = False,
    elementTypes = [
        pPageType, -- link to next page
        ArrayType {
            nArrayElements = pageSize,
            elementType = nodeType
            }
        ]
    }))

frameTypeName :: Name
frameTypeName = "frame"

frameType :: Type
frameType = NamedTypeReference frameTypeName

pFrameType :: Type
pFrameType = ptr frameType

frameTypedef :: ModuleBuilder ()
frameTypedef = typedef frameTypeName (Just (StructureType {
    isPacked = False,
    elementTypes = [
        pFrameType, -- link to caller frame (for gc and getting parameters by reference)
        i32, -- number of vars
        ppNodeType, -- array of vars (stack allocated)
        i32, -- number of DO EDGES iterators
        pDoEdgesIteratorType, -- array of DO EDGES iterators (stack allocated)
        i32, -- number of call arguments
        pppNodeType -- array of call arguments (stack allocated)
        ]
    }))

doEdgesIteratorTypeName :: Name
doEdgesIteratorTypeName = "doEdgesIterator"

doEdgesIteratorType :: Type
doEdgesIteratorType = NamedTypeReference doEdgesIteratorTypeName

pDoEdgesIteratorType :: Type
pDoEdgesIteratorType = ptr doEdgesIteratorType

doEdgesIteratorTypedef :: ModuleBuilder ()
doEdgesIteratorTypedef = typedef doEdgesIteratorTypeName (Just (StructureType {
    isPacked = False,
    elementTypes = [
        i32, -- iterator index
        i32, -- size of allocated array of edges
        ppNodeType -- array of edges
        ]
    }))
