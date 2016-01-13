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
     astTypeName,astTypeSize,astTypeImportSize,astTypeIsImport,
     astFuncName,
     astStmtSourcePos)

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
  | BloBitType
data BloFunc rt =
    BloFunc (BloFuncSig rt) (BloStmt rt)
  | BloRtFunc (BloFuncSig rt) rt
data BloFuncSig rt = BloFuncSig [(String,BloType rt)] (Maybe (BloType rt))
data BloStmtInfo rt = BloStmtInfo [(String,BloType rt)] (Maybe (BloStmt rt))
data BloStmt rt =
    BloStmtBlock (BloStmtInfo rt) [BloStmt rt]
  | BloStmtVar (BloStmtInfo rt) String (BloType rt) (Maybe (BloExpr rt))
  | BloStmtIf (BloStmtInfo rt) (BloExpr rt) (BloStmt rt) (Maybe (BloStmt rt))
  | BloStmtBreak (BloStmtInfo rt)
  | BloStmtReturn (BloStmtInfo rt) (BloExpr rt)
  | BloStmtSetClear (BloStmtInfo rt) Bool (BloExpr rt)
  | BloStmtAssign (BloStmtInfo rt) (BloExpr rt) (BloExpr rt)
  | BloStmtExpr (BloStmtInfo rt) (BloExpr rt)
data BloExpr rt =
    BloExprVar String (BloType rt)
  | BloExprFunc (BloFunc rt)
  | BloExprField Int Int (BloType rt) (BloExpr rt)

annotateTypes :: Runtime rt => [AstType] -> Annotate (Map String (BloType rt))
annotateTypes astTypes = do
    annotated <- mapM addAnnotation astTypes
    return (fromList annotated)
  where
    addAnnotation astType
      | astTypeIsImport astType = do
            rt <- Annotate (annotateType astType)
            return (astTypeName astType,
                    BloRtType (astTypeSize astType)
                              (astTypeImportSize astType) rt)
      | otherwise =
            return (astTypeName astType,
                    BloType (astTypeSize astType) (astTypeImportSize astType))

annotateFuncs :: Runtime rt => Map String (BloType rt) -> [AstFunc] -> Annotate (Map String (BloFunc rt))
annotateFuncs types astFuncs = do
    annotated <- mapM addAnnotation astFuncs
    return (toBlo (fromList annotated))
  where
    addAnnotation astFunc@(AstFunc astFuncSig astStmt) =
        return (astFuncName astFunc,(toBloFuncSig astFuncSig,Left astStmt))
    addAnnotation astFunc@(AstImportFunc astFuncSig) = do
        rt <- Annotate (annotateFunc astFuncSig)
        return (astFuncName astFunc,(toBloFuncSig astFuncSig,Right rt))

    toBloType AstTypeBit = BloBitType
    toBloType astType = types M.! astTypeName astType

    toBloFuncSig (AstFuncSig _ _ vars retType) =
        BloFuncSig (map (fmap toBloType) vars) (fmap toBloType retType)

    toBlo :: Map String (BloFuncSig rt,Either AstStmt rt) -> Map String (BloFunc rt)
    toBlo annotated = bloFuncs
      where
        bloFuncs = M.map toBloFunc annotated

        toBloFunc :: (BloFuncSig rt,Either AstStmt rt) -> BloFunc rt
        toBloFunc (bloFuncSig,Right rt) = BloRtFunc bloFuncSig rt
        toBloFunc (bloFuncSig,Left stmt) = BloFunc bloFuncSig (toBloStmt stmt)

        toBloStmt :: AstStmt -> BloStmt rt
        toBloStmt = undefined
