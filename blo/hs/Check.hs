module Check
    (Ast(..),AstType(..),AstFunc(..),AstFuncSig(..),AstStmt(..),AstExpr(..),
     check)
where

import Control.Monad(foldM_)
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
        funcs <- checkFuncs types funcSigs
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
     AstType SourcePos String Int (String -> Maybe (Int,AstType))
   | AstImportType SourcePos String Int (String -> Maybe (Int,AstType))
data AstFunc =
    AstFunc AstFuncSig AstStmt
  | AstImportFunc AstFuncSig
data AstFuncSig = AstFuncSig SourcePos String [AstType] (Maybe AstType)
data AstStmt = AstStmt
data AstExpr = AstExpr

astTypeName :: AstType -> String
astTypeName (AstType _ name _ _) = name
astTypeName (AstImportType _ name _ _) = name

astTypeSize :: AstType -> Int
astTypeSize (AstType _ _ size _) = size
astTypeSize (AstImportType _ _ size _) = size

checkTypes :: [Definition] -> Check (Map String AstType)
checkTypes defs = do
    astTypes <- sequence (elems checkedTypes)
    return (fromList (map (\ t -> (astTypeName t,t)) astTypes))
  where
    checkedTypes = fromList (concatMap checkDef defs)
    checkDef (FuncDef _ _ _) = []
    checkDef (FuncImport _ _) = []
    checkDef (TypeDef pos (Identifier _ name) fields) =
        [(name,checkType AstType pos name fields)]
    checkDef (TypeImport pos (Identifier _ name) fields) =
        [(name,checkType AstImportType pos name fields)]
    checkType astType pos name fields = undefined

checkFuncSigs :: [Definition] -> Map String AstType -> Check (Map String AstFuncSig)
checkFuncSigs = undefined

checkFuncs :: Map String AstType -> Map String AstFuncSig -> Check (Map String AstFunc)
checkFuncs = undefined
