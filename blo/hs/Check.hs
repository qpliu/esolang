module Check
    (Ast(..),AstType(..),AstFunc(..),AstFuncSig(..),AstStmt(..),AstExpr(..),
     astTypeName,astTypeSize,astTypeField,
     astFuncName,astFuncParams,astFuncType,
     check)
where

import Control.Monad(foldM,foldM_,liftM)
import Data.Map(Map,elems,empty,fromList,insert,member)
import qualified Data.Map as M

import Parse
    (Error(..),SourcePos,
     Identifier(..),Definition(..),
     FuncHeader(..),TypeField(..),Var(..),
     Stmt(..),Expr(..))

check :: [Definition] -> Either Error Ast
check defs = let Check result = runCheck in result
  where
    runCheck = do
        checkDuplicateTypes defs
        checkDuplicateFuncs defs
        types <- checkTypes defs
        funcSigs <- checkFuncSigs defs types
        funcs <- checkFuncs defs types funcSigs
        return (Ast (flip M.lookup types) (flip M.lookup funcs))

data Check a = Check (Either Error a)
  deriving Show

instance Monad Check where
    (Check a) >>= f = either (Check . Left) f a
    return = Check . Right
    fail = error
    
checkError :: SourcePos -> String -> Check a
checkError pos msg = (Check . Left . Error pos) msg

checkDuplicateTypes :: [Definition] -> Check ()
checkDuplicateTypes defs = checkDuplicates "type" (concatMap t defs)
  where
    t (TypeDef pos (Identifier _ name) _) = [(pos,name)]
    t (TypeImport pos (Identifier _ name) _) = [(pos,name)]
    t _ = []

checkDuplicateFuncs :: [Definition] -> Check ()
checkDuplicateFuncs defs = checkDuplicates "func" (concatMap f defs)
  where
    f (FuncDef pos (FuncHeader (Identifier _ name) _ _) _) = [(pos,name)]
    f (FuncImport pos (FuncHeader (Identifier _ name) _ _)) = [(pos,name)]
    f _ = []

checkDuplicates :: String -> [(SourcePos,String)] -> Check ()
checkDuplicates label items = foldM_ checkItem empty items
  where
    checkItem set (pos,name)
      | member name set =
            checkError pos ("Duplicate " ++ label ++ " '" ++ name ++ "'")
      | otherwise = return (insert name () set)

data Ast = Ast (String -> Maybe AstType) (String -> Maybe AstFunc)

data AstType =
     AstType SourcePos String Int (String -> Maybe (Int,Maybe AstType))
   | AstImportType SourcePos String Int (String -> Maybe (Int,Maybe AstType))
data AstFunc =
    AstFunc AstFuncSig AstStmt
  | AstImportFunc AstFuncSig
data AstFuncSig = AstFuncSig SourcePos String [AstVar] (Maybe AstType)
data AstVar = AstVar String AstType
data AstStmt = AstStmt
data AstExpr = AstExpr

instance Eq AstType where
    t1 == t2 = astTypeName t1 == astTypeName t2

astTypeName :: AstType -> String
astTypeName (AstType _ name _ _) = name
astTypeName (AstImportType _ name _ _) = name

astTypeSize :: Maybe AstType -> Int
astTypeSize Nothing = 1
astTypeSize (Just (AstType _ _ size _)) = size
astTypeSize (Just (AstImportType _ _ size _)) = size

astTypeField :: AstType -> String -> Maybe (Int,Maybe AstType)
astTypeField (AstType _ _ _ getField) = getField
astTypeField (AstImportType _ _ _ getField) = getField

astFuncName :: AstFunc -> String
astFuncName (AstFunc (AstFuncSig _ name _ _) _) = name
astFuncName (AstImportFunc (AstFuncSig _ name _ _)) = name

astFuncParams :: AstFunc -> [AstType]
astFuncParams (AstFunc (AstFuncSig _ _ params _) _) =
    map (\ (AstVar _ astType) -> astType) params
astFuncParams (AstImportFunc (AstFuncSig _ _ params _)) =
    map (\ (AstVar _ astType) -> astType) params

astFuncType :: AstFunc -> Maybe AstType
astFuncType (AstFunc (AstFuncSig _ _ _ returnType) _) = returnType
astFuncType (AstImportFunc (AstFuncSig _ _ _ returnType)) = returnType

checkTypes :: [Definition] -> Check (Map String AstType)
checkTypes defs = do
    foldM_ checkDuplicateFieldNames empty (elems uncheckedTypes)
    mapM_ (checkFieldTypes empty) (elems uncheckedTypes)
    return checkedTypes
  where
    uncheckedTypes = fromList (concatMap uncheckedDef defs)
    uncheckedDef (FuncDef _ _ _) = []
    uncheckedDef (FuncImport _ _) = []
    uncheckedDef (TypeDef pos (Identifier _ name) fields) =
        [(name,(AstType,pos,name,fields))]
    uncheckedDef (TypeImport pos (Identifier _ name) fields) =
        [(name,(AstImportType,pos,name,fields))]

    checkDuplicateFieldNames set (_,pos,name,_)
      | member name set =
            checkError pos ("Duplicate field name '" ++ name ++ "'")
      | otherwise = return (insert name () set)

    checkFieldTypes set (_,pos,name,fields)
      | member name set =
            checkError pos ("Recursively defined type '" ++ name ++ "'")
      | otherwise = mapM_ (checkField (insert name () set)) fields
      where
        checkField set (TypeField _ Nothing) = return ()
        checkField set (TypeField _ (Just (Identifier pos typeName))) =
            maybe (checkError pos ("Unknown type '" ++ typeName ++ "'"))
                  (checkFieldTypes set) (M.lookup typeName uncheckedTypes)

    checkedTypes = fromList (map checkType (elems uncheckedTypes))
    checkType (astType,pos,name,fields) = (name,astType pos name size getField)
      where
        checkedFieldTypes = map lookupFieldType fields
          where
            lookupFieldType (TypeField (Identifier _ fieldName) Nothing) =
                (fieldName,Nothing)
            lookupFieldType (TypeField (Identifier _ fieldName) (Just (Identifier _ typeName))) =
                (fieldName,M.lookup typeName checkedTypes)
        size = sum (map (astTypeSize . snd) checkedFieldTypes)
        getField fieldName = M.lookup fieldName fieldMap
          where
            (_,fieldMap) = foldl addFieldOffset (0,empty) checkedFieldTypes
            addFieldOffset (offset,fieldMap) (fieldName,fieldType) =
                (offset + astTypeSize fieldType,insert fieldName (offset,fieldType) fieldMap)

checkFuncSigs :: [Definition] -> Map String AstType -> Check (Map String AstFuncSig)
checkFuncSigs defs types = foldM checkDef empty defs
  where
    checkDef funcSigs (FuncDef pos funcHeader _) = checkSig funcSigs pos funcHeader
    checkDef funcSigs (FuncImport pos funcHeader) = checkSig funcSigs pos funcHeader
    checkDef funcSigs _ = return funcSigs
    checkSig funcSigs pos (FuncHeader (Identifier _ name) params returnType) = do
        astFuncType <- maybe (return Nothing) (liftM Just . checkType) returnType
        foldM_ checkDuplicateVar empty params
        astParams <- mapM checkVar params
        return (insert name (AstFuncSig pos name astParams astFuncType) funcSigs)
    checkType (Identifier pos typeName) =
        maybe (checkError pos ("Unknown type '" ++ typeName ++ "'"))
              return (M.lookup typeName types)
    checkDuplicateVar set (Var (Identifier pos name) _)
      | member name set =
            checkError pos ("Duplicate parameter '" ++ name ++ "'")
      | otherwise = return (insert name () set)
    checkVar (Var (Identifier _ name) paramType) = do
        astParamType <- checkType paramType
        return (AstVar name astParamType)

checkFuncs :: [Definition] -> Map String AstType -> Map String AstFuncSig -> Check (Map String AstFunc)
checkFuncs = undefined
