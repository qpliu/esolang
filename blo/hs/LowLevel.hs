module LowLevel
    (Type(..),Func(..),FuncSig(..),StmtInfo(..),Stmt(..),Expr(..),
     stmtInfo,stmtLeavingScope,stmtNext,stmtScope,
     toLowLevel)
where

import Data.Map(Map,empty,fromList,insert,toList)
import qualified Data.Map as M

import Compile(Compile,SourcePos,compileError)
import Check
        (AstType(..),AstFuncSig(..),AstStmt(..),AstExpr(..),
         astTypeName,astTypeSize,astTypeFields,astTypeImportSize,
         astStmtSourcePos)
import Runtime(RuntimeAst(..))

toLowLevel :: RuntimeAst rtt rtf -> ([(String,Type rtt)],
                                     [(String,Func rtt rtf)])
toLowLevel (RuntimeAst rtTypes rtFuncs) = (toList types,toList funcs)
  where
    types = toLowLevelTypes rtTypes
    funcs = toLowLevelFuncs types rtFuncs

data Type rtt = Type Int [rtt]
data Func rtt rtf =
    Func (FuncSig rtt rtf) (Stmt rtt rtf) Int
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
stmtInfo (StmtFor info _) = info
stmtInfo (StmtBreak info) = info
stmtInfo (StmtReturn info _) = info
stmtInfo (StmtSetClear info _ _) = info
stmtInfo (StmtAssign info _ _) = info
stmtInfo (StmtExpr info _) = info

stmtNext :: Stmt rtt rtf -> Maybe (Stmt rtt rtf)
stmtNext stmt = let (StmtInfo _ next) = stmtInfo stmt in next

stmtScope :: Stmt rtt rtf -> [(String,Type rtt)]
stmtScope stmt = let (StmtInfo scope _) = stmtInfo stmt in scope

stmtLeavingScope :: Stmt rtt rtf -> Maybe (Stmt rtt rtf) -> [(String,Type rtt)]
stmtLeavingScope stmt nextStmt =
    maybe (stmtScope stmt) (subtract (stmtScope stmt) . stmtScope) nextStmt
  where
    subtract scope newScope =
        filter (maybe True (const False) . flip lookup newScope . fst) scope

toLowLevelTypes :: [(AstType,Maybe rtt)] -> Map String (Type rtt)
toLowLevelTypes astTypes =
    fromList (("",Type 1 []) : map toLowLevelType astTypes)
  where
    rttMap = fromList (("",[]) : map toRttMap astTypes)
    toRttMap (astType,Nothing) = (astTypeName astType,[])
    toRttMap (astType,Just rtt) = (astTypeName astType,[rtt])
    astTypeRtt (_,(_,_,fieldAstType)) = rttMap M.! astTypeName fieldAstType
    toLowLevelType (astType,rtt) =
        (astTypeName astType,
         Type (astTypeSize astType)
              ((maybe id (:) rtt)
                  (concatMap astTypeRtt (astTypeFields astType))))

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
        (name,Func (toFuncSig astFuncSig) (getStmt astStmt) maxAliases)
      where
        (stmts,maxAliases,_) = addStmt empty (empty,0,params) (astStmt,Nothing)
        getStmt astStmt = stmts M.! astStmtSourcePos astStmt

        addStmt breakLabels (stmtDict,maxAlias,scope) (astStmt,nextAstStmt) =
            addAstStmt astStmt
          where
            stmtInfo = StmtInfo (map (fmap getType) scope)
                                (fmap getStmt nextAstStmt)
            addAstStmt (AstStmtBlock pos []) =
                (insert pos (StmtBlock stmtInfo []) stmtDict,maxAlias,scope)
            addAstStmt (AstStmtBlock pos astStmts) =
                let info = StmtInfo (map (fmap getType) scope)
                                    ((Just . getStmt . head) astStmts)
                    stmts = map getStmt astStmts
                    (newDict,maxAlias2,_) =
                        foldl (addStmt breakLabels)
                              (stmtDict,maxAlias,scope)
                              (zip astStmts
                                   (map Just (drop 1 astStmts)
                                    ++ [nextAstStmt]))
                in  (insert pos (StmtBlock info stmts) newDict,maxAlias2,scope)
            addAstStmt (AstStmtVar pos (name,astType) astExpr) =
                (insert pos (StmtVar stmtInfo name (getType astType)
                                     (fmap toExpr astExpr))
                        stmtDict,
                 max maxAlias newMaxAlias,
                 (name,astType):scope)
              where
                newMaxAlias
                  | M.null breakLabels = maxAlias
                  | otherwise = length (filter (canAlias astType)
                                               (map snd scope))
            addAstStmt (AstStmtIf pos astExpr astIf astElse) =
                let (dictIf,maxAlias2,_) =
                        addStmt breakLabels (stmtDict,maxAlias,scope)
                                (astIf,nextAstStmt)
                    (dictElse,maxAlias3,_) =
                        maybe (dictIf,maxAlias2,scope)
                              (addStmt breakLabels (dictIf,maxAlias2,scope)
                               . flip (,) nextAstStmt)
                              astElse
                in  (insert pos (StmtIf stmtInfo (toExpr astExpr)
                                        (getStmt astIf)
                                        (fmap getStmt astElse))
                            dictElse,
                     maxAlias3,
                     scope)
            addAstStmt astForStmt@(AstStmtFor pos label astStmt) =
                let (dictFor,maxAlias2,_) =
                        addStmt (insert label nextAstStmt
                                        (insert Nothing nextAstStmt
                                                breakLabels))
                                (stmtDict,maxAlias,scope)
                                (astStmt,Just astForStmt)
                in  (insert pos (StmtFor stmtInfo (getStmt astStmt)) dictFor,
                     maxAlias2,
                     scope)
            addAstStmt (AstStmtBreak pos label) =
                let info = StmtInfo (map (fmap getType) scope)
                                    (fmap getStmt (breakLabels M.! label))
                in  (insert pos (StmtBreak info) stmtDict,maxAlias,scope)
            addAstStmt (AstStmtReturn pos astExpr) =
                (insert pos (StmtReturn stmtInfo (fmap toExpr astExpr))
                        stmtDict,
                 maxAlias,
                 scope)
            addAstStmt (AstStmtSetClear pos bit astExpr) =
                (insert pos (StmtSetClear stmtInfo bit (toExpr astExpr))
                        stmtDict,
                 maxAlias,
                 scope)
            addAstStmt (AstStmtAssign pos lhs rhs) =
                (insert pos (StmtAssign stmtInfo (toExpr lhs) (toExpr rhs))
                        stmtDict,
                 maxAlias,
                 scope)
            addAstStmt (AstStmtExpr pos astExpr) =
                (insert pos (StmtExpr stmtInfo (toExpr astExpr)) stmtDict,
                 maxAlias,
                 scope)

canAlias :: AstType -> AstType -> Bool
canAlias astType1 astType2 =
    astType1 == astType2 || any (flip canAlias astType2) fieldTypes
  where
    fieldTypes = map (\ (_,(_,_,astType)) -> astType) (astTypeFields astType1)
