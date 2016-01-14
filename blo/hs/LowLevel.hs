module LowLevel
    (Type(..),Func(..),FuncSig(..),StmtInfo(..),Stmt(..),Expr(..),
     stmtInfo,stmtLeavingScope,stmtNext,stmtScope,
     toLowLevel)
where

import Data.Map(Map,empty,fromList,insert)
import qualified Data.Map as M

import Compile(Compile,SourcePos,compileError)
import Check
        (AstType(..),AstFuncSig(..),AstStmt(..),AstExpr(..),
         astTypeName,astTypeSize,astTypeImportSize,astStmtSourcePos)
import Runtime(RuntimeAst(..))

toLowLevel :: RuntimeAst rtt rtf -> String -> Maybe (Func rtt rtf)
toLowLevel (RuntimeAst rtTypes rtFuncs) = flip M.lookup funcs
  where
    types = fromList (map toLowLevelType rtTypes)
    funcs = toLowLevelFuncs types rtFuncs

data Type rtt = Type Int Int (Maybe rtt)
data Func rtt rtf =
    Func (FuncSig rtt rtf) (Stmt rtt rtf)
  | ImportFunc (FuncSig rtt rtf) rtf
data FuncSig rtt rtf = FuncSig [(String,Type rtt)] (Maybe (Type rtt))
data StmtInfo rtt rtf = StmtInfo [(String,Type rtt)] (Maybe (Stmt rtt rtf))
data Stmt rtt rtf =
    StmtBlock    (StmtInfo rtt rtf) [Stmt rtt rtf]
  | StmtVar      (StmtInfo rtt rtf) String (Type rtt) (Maybe (Expr rtt rtf))
  | StmtIf       (StmtInfo rtt rtf) (Expr rtt rtf)
                                    (Stmt rtt rtf) (Maybe (Stmt rtt rtf))
  | StmtFor      (StmtInfo rtt rtf) (Stmt rtt rtf)
  | StmtBreak    (StmtInfo rtt rtf)
  | StmtReturn   (StmtInfo rtt rtf) (Maybe (Expr rtt rtf))
  | StmtSetClear (StmtInfo rtt rtf) Bool (Expr rtt rtf)
  | StmtAssign   (StmtInfo rtt rtf) (Expr rtt rtf) (Expr rtt rtf)
  | StmtExpr     (StmtInfo rtt rtf) (Expr rtt rtf)
data Expr rtt rtf =
    ExprVar String
  | ExprFunc (Func rtt rtf) [Expr rtt rtf]
  | ExprField Int Int (Type rtt) (Expr rtt rtf)

stmtInfo :: Stmt rtt rtf -> StmtInfo rtt rtf
stmtInfo (StmtBlock info _) = info
stmtInfo (StmtVar info _ _ _) = info
stmtInfo (StmtIf info _ _ _) = info
stmtInfo (StmtBreak info) = info
stmtInfo (StmtReturn info _) = info
stmtInfo (StmtSetClear info _ _) = info
stmtInfo (StmtAssign info _ _) = info
stmtInfo (StmtExpr info _) = info

stmtNext :: Stmt rtt rtf -> Maybe (Stmt rtt rtf)
stmtNext stmt = let (StmtInfo _ next) = stmtInfo stmt in next

stmtScope :: Stmt rtt rtf -> [(String,Type rtt)]
stmtScope stmt = let (StmtInfo scope _) = stmtInfo stmt in scope

stmtLeavingScope :: Stmt rtt rtf -> [(String,Type rtt)]
stmtLeavingScope stmt =
    maybe (stmtScope stmt) (subtract (stmtScope stmt) . stmtScope)
          (stmtNext stmt)
  where
    subtract scope newScope =
        filter (maybe False (const True) . flip lookup newScope . fst) scope

toLowLevelType :: (AstType,Maybe rtt) -> (String,Type rtt)
toLowLevelType (astType,rtt) =
    (astTypeName astType,
     Type (astTypeSize astType) (astTypeImportSize astType) rtt)

toLowLevelFuncs :: Map String (Type rtt) -> [(AstFuncSig,Either AstStmt rtf)]
                                         -> Map String (Func rtt rtf)
toLowLevelFuncs types rtFuncs = funcs
  where
    funcs = fromList (map toFunc rtFuncs)
    getType astType = types M.! astTypeName astType
    toFuncSig (AstFuncSig _ _ params retType) =
        FuncSig (map (fmap getType) params) (fmap getType retType)
    getFunc (AstFuncSig _ name _ _) = funcs M.! name

    toExpr (AstExprVar name astType) = ExprVar name
    toExpr (AstExprFunc astFuncSig astExprs) =
        ExprFunc (getFunc astFuncSig) (map toExpr astExprs)
    toExpr (AstExprField offset importOffset astType astExpr) =
        ExprField offset importOffset (getType astType) (toExpr astExpr)

    toFunc (astFuncSig@(AstFuncSig _ name _ _),Right rtf) =
        (name,ImportFunc (toFuncSig astFuncSig) rtf)
    toFunc (astFuncSig@(AstFuncSig _ name params _),Left astStmt) =
        (name,Func (toFuncSig astFuncSig) (getStmt astStmt))
      where
        (stmts,_) = addStmt empty (empty,(map (fmap getType) params))
                                  (astStmt,Nothing)
        getStmt astStmt = stmts M.! astStmtSourcePos astStmt

        addStmt breakLabels (stmtDict,scope) (astStmt,nextAstStmt) =
            addAstStmt astStmt
          where
            stmtInfo = StmtInfo scope (fmap getStmt nextAstStmt)
            addAstStmt (AstStmtBlock pos []) =
                (insert pos (StmtBlock stmtInfo []) stmtDict,scope)
            addAstStmt (AstStmtBlock pos astStmts) =
                let info = StmtInfo scope ((Just . getStmt . head) astStmts)
                    stmts = map getStmt astStmts
                    (newDict,_) = foldl (addStmt breakLabels) (stmtDict,scope)
                                        (zip astStmts
                                             (map Just (drop 1 astStmts)
                                              ++ [nextAstStmt]))
                in  (insert pos (StmtBlock info stmts) newDict,scope)
            addAstStmt (AstStmtVar pos (name,astType) astExpr) =
                (insert pos (StmtVar stmtInfo name (getType astType)
                                     (fmap toExpr astExpr))
                        stmtDict,
                 (name,getType astType):scope)
            addAstStmt (AstStmtIf pos astExpr astIf astElse) =
                let (dictIf,_) = addStmt breakLabels (stmtDict,scope)
                                         (astIf,nextAstStmt)
                    (dictElse,_) = maybe (dictIf,scope)
                                         (addStmt breakLabels (dictIf,scope)
                                          . flip (,) nextAstStmt) astElse
                in  (insert pos (StmtIf stmtInfo (toExpr astExpr)
                                        (getStmt astIf)
                                        (fmap getStmt astElse))
                            dictElse,
                     scope)
            addAstStmt astForStmt@(AstStmtFor pos label astStmt) =
                let (dictFor,_) = addStmt (insert label nextAstStmt
                                                  (insert Nothing nextAstStmt
                                                          breakLabels))
                                          (stmtDict,scope)
                                          (astStmt,Just astForStmt)
                in  (insert pos (StmtFor stmtInfo (getStmt astStmt)) dictFor,
                     scope)
            addAstStmt (AstStmtBreak pos label) =
                let info = StmtInfo scope
                                    (fmap getStmt (breakLabels M.! label))
                in  (insert pos (StmtBreak info) stmtDict,scope)
            addAstStmt (AstStmtReturn pos astExpr) =
                (insert pos (StmtReturn stmtInfo (fmap toExpr astExpr))
                        stmtDict,
                 scope)
            addAstStmt (AstStmtSetClear pos bit astExpr) =
                (insert pos (StmtSetClear stmtInfo bit (toExpr astExpr))
                        stmtDict,
                 scope)
            addAstStmt (AstStmtAssign pos lhs rhs) =
                (insert pos (StmtAssign stmtInfo (toExpr lhs) (toExpr rhs))
                        stmtDict,
                 scope)
            addAstStmt (AstStmtExpr pos astExpr) =
                (insert pos (StmtExpr stmtInfo (toExpr astExpr)) stmtDict,
                 scope)
