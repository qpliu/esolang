module Annotate
    (Blo,
     annotate)
where

import Control.Applicative(Applicative(..))
import Data.Map(Map,fromList)
import qualified Data.Map as M

import Parse(Error(..),SourcePos)
import Check
    (Ast(..),AstType(..),AstFunc(..),AstFuncSig(..),AstStmt(..),AstExpr(..),
     AstVar,
     astTypeName,astTypeSize,astTypeImportSize,astTypeIsImport,
     astFuncName,astFuncParams,astFuncType,
     astVarName,astVarType,
     astExprType)

class Runtime rt where
    annotateType :: AstType -> Either Error rt
    annotateFunc :: AstFuncSig -> Either Error rt

annotate :: Runtime rt => Ast -> Either Error (Blo rt)
annotate (Ast astTypes astFuncs) = let Annotate result = doAnnotate in result
  where
    doAnnotate = do
        types <- annotateTypes astTypes
        funcs <- annotateFuncs types astFuncs
        return (flip M.lookup funcs)

data Annotate a = Annotate (Either Error a)

instance Monad Annotate where
    (Annotate a) >>= f = either (Annotate . Left) f a
    return = Annotate . Right
    fail = error

instance Functor Annotate where
    fmap f (Annotate a) = Annotate (either Left (Right . f) a)

instance Applicative Annotate where
    pure = return
    f <*> a = f >>= ($ a) . fmap
    a *> b = a >> b
    a <* b = a >>= (b >>) . return

annotateError :: SourcePos -> String -> Annotate a
annotateError pos msg = (Annotate . Left . Error pos) msg

type Blo rt = String -> Maybe (BloFunc rt)
data BloType rt =
    BloType Int Int
  | BloRtType Int Int rt
data BloFunc rt =
    BloFunc (BloFuncSig rt) (BloStmt rt)
  | BloRtFunc (BloFuncSig rt) rt
data BloFuncSig rt = BloFuncSig [(String,BloType rt)] (Maybe (BloType rt))
type BloNext rt = Maybe (BloStmt rt)
data BloStmt rt =
    BloStmtBlock (BloNext rt) [BloStmt rt]
  | BloStmtVar (BloNext rt) String (BloType rt) (Maybe (BloExpr rt))
  | BloStmtIf (BloNext rt) (BloExpr rt) (BloStmt rt) (Maybe (BloStmt rt))
  | BloStmtBreak (BloNext rt)
  | BloStmtReturn (BloExpr rt)
  | BloStmtSetClear (BloNext rt) Bool (BloExpr rt)
  | BloStmtAssign (BloNext rt) (BloExpr rt) (BloExpr rt)
  | BloStmtExpr (BloNext rt) (BloExpr rt)
data BloExpr rt =
    BloExprVar String (BloType rt)
  | BloExprFunc (BloFunc rt)
  | BloExprField Int Int (BloType rt) (BloExpr rt)

annotateTypes :: Runtime rt => [AstType] -> Annotate (Map String (BloType rt))
annotateTypes astTypes = do
    bloTypes <- mapM rtAnnotate astTypes
    return (fromList bloTypes)
  where
    rtAnnotate astType
      | astTypeIsImport astType = do
            rt <- Annotate (annotateType astType)
            return (astTypeName astType,
                    BloRtType (astTypeSize astType)
                              (astTypeImportSize astType) rt)
      | otherwise =
            return (astTypeName astType,
                    BloType (astTypeSize astType) (astTypeImportSize astType))

annotateFuncs :: Runtime rt => Map String (BloType rt) -> [AstFunc] -> Annotate (Map String (BloFunc rt))
annotateFuncs types astFuncs = undefined
