module LowLevel
    (Type(..),Func(..),FuncSig(..),StmtInfo(..),Stmt(..),Expr(..),InsnId,
     funcMaxAliases,funcType,
     stmtLeavingScope,stmtNext,stmtScope,stmtId,stmtNextId,
     toLowLevel)
where

import Data.Map(Map,empty,fromList,insert,toList)
import qualified Data.Map as M

import Compile(SourcePos)
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
    Func (FuncSig rtt rtf) (Stmt rtt rtf)
  | ImportFunc (FuncSig rtt rtf) rtf
data FuncSig rtt rtf = FuncSig [(String,Type rtt)] (Maybe (Type rtt))
data StmtInfo rtt rtf =
    StmtInfo [(String,Type rtt)] (Maybe (Stmt rtt rtf)) InsnId
data Stmt rtt rtf =
    StmtBlock    (StmtInfo rtt rtf) [Stmt rtt rtf]
  | StmtVar      (StmtInfo rtt rtf) String (Type rtt) Int
                                    (Maybe (Expr rtt rtf))
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
  | ExprFunc String (Func rtt rtf) [Expr rtt rtf] (Int,InsnId)
  | ExprField Int Int (Type rtt) (Expr rtt rtf)
type InsnId = SourcePos

stmtInfo :: Stmt rtt rtf -> StmtInfo rtt rtf
stmtInfo (StmtBlock info _) = info
stmtInfo (StmtVar info _ _ _ _) = info
stmtInfo (StmtIf info _ _ _) = info
stmtInfo (StmtFor info _) = info
stmtInfo (StmtBreak info) = info
stmtInfo (StmtReturn info _) = info
stmtInfo (StmtSetClear info _ _) = info
stmtInfo (StmtAssign info _ _) = info
stmtInfo (StmtExpr info _) = info

stmtNext :: Stmt rtt rtf -> Maybe (Stmt rtt rtf)
stmtNext stmt = let (StmtInfo _ next _) = stmtInfo stmt in next

stmtScope :: Stmt rtt rtf -> [(String,Type rtt)]
stmtScope stmt = let (StmtInfo scope _ _) = stmtInfo stmt in scope

stmtLeavingScope :: Stmt rtt rtf -> Maybe (Stmt rtt rtf) -> [(String,Type rtt)]
stmtLeavingScope stmt nextStmt =
    maybe (stmtScopeOnExit stmt)
          (subtract (stmtScopeOnExit stmt) . stmtScope) nextStmt
  where
    subtract scope newScope =
        filter (maybe True (const False) . flip lookup newScope . fst) scope
    stmtScopeOnExit (StmtVar _ name varType _ _) =
        (name,varType):stmtScope stmt
    stmtScopeOnExit stmt = stmtScope stmt

stmtId :: Stmt rtt rtf -> InsnId
stmtId stmt = let (StmtInfo _ _ insnId) = stmtInfo stmt in insnId

stmtNextId :: Stmt rtt rtf -> Maybe InsnId
stmtNextId stmt = (fmap stmtId . stmtNext) stmt

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

    toExpr = toAliasableExpr []
    toAliasableExpr scope (AstExprVar name astType) = ExprVar name
    toAliasableExpr scope (AstExprField offset importOffset astType astExpr) =
        ExprField offset importOffset (getType astType)
                  (toAliasableExpr scope astExpr)
    toAliasableExpr scope
                    (AstExprFunc pos astFuncSig@(AstFuncSig _ funcName _ 
                                                            (Just retType))
                                 astExprs) =
        ExprFunc funcName (getFunc astFuncSig) (map toExpr astExprs)
                 (maxAliases,pos)
      where
        maxAliases = 1 + length (filter (canAlias retType) (map snd scope))
    toAliasableExpr scope (AstExprFunc pos 
                                       astFuncSig@(AstFuncSig _ funcName _ _)
                                       astExprs) =
        ExprFunc funcName (getFunc astFuncSig) (map toExpr astExprs) (1,pos)

    toFunc (astFuncSig@(AstFuncSig _ name _ _),Right rtf) =
        (name,ImportFunc (toFuncSig astFuncSig) rtf)
    toFunc (astFuncSig@(AstFuncSig _ name params _),Left astStmt) =
        (name,Func (toFuncSig astFuncSig) (getStmt astStmt))
      where
        (stmts,_) = addStmt empty (empty,params) (astStmt,Nothing)
        getStmt astStmt = stmts M.! astStmtSourcePos astStmt

        addStmt breakLabels (stmtDict,scope) (astStmt,nextAstStmt) =
            addAstStmt astStmt
          where
            stmtInfo = StmtInfo (map (fmap getType) scope)
                                (fmap getStmt nextAstStmt)
            addAstStmt (AstStmtBlock pos []) =
                (insert pos (StmtBlock (stmtInfo pos) []) stmtDict,
                 scope)
            addAstStmt (AstStmtBlock pos astStmts) =
                let info = StmtInfo (map (fmap getType) scope)
                                    ((Just . getStmt . head) astStmts)
                                    pos
                    stmts = map getStmt astStmts
                    (newDict,_) =
                        foldl (addStmt breakLabels)
                              (stmtDict,scope)
                              (zip astStmts
                                   (map Just (drop 1 astStmts)
                                    ++ [nextAstStmt]))
                in  (insert pos (StmtBlock info stmts) newDict,scope)
            addAstStmt (AstStmtVar pos (name,astType) astExpr) =
                (insert pos (StmtVar (stmtInfo pos) name (getType astType)
                                     varMaxAlias
                                     (fmap (toAliasableExpr aliasScope)
                                           astExpr))
                        stmtDict,
                 newScope)
              where
                varMaxAlias
                  | M.null breakLabels = 1
                  | otherwise = 1 + length (filter (canAlias astType)
                                                   (map snd scope))
                newScope = (name,astType):scope
                aliasScope | M.null breakLabels = [] | otherwise = newScope
            addAstStmt (AstStmtIf pos astExpr astIf astElse) =
                let (dictIf,_) = addStmt breakLabels (stmtDict,scope)
                                         (astIf,nextAstStmt)
                    (dictElse,_) =
                        maybe (dictIf,scope)
                              (addStmt breakLabels (dictIf,scope)
                               . flip (,) nextAstStmt)
                              astElse
                in  (insert pos (StmtIf (stmtInfo pos) (toExpr astExpr)
                                        (getStmt astIf)
                                        (fmap getStmt astElse))
                            dictElse,
                     scope)
            addAstStmt astForStmt@(AstStmtFor pos label astStmt) =
                let (dictFor,_) =
                        addStmt (insert label nextAstStmt
                                        (insert Nothing nextAstStmt
                                                breakLabels))
                                (stmtDict,scope)
                                (astStmt,Just astForStmt)
                in  (insert pos (StmtFor (stmtInfo pos) (getStmt astStmt))
                            dictFor,
                     scope)
            addAstStmt (AstStmtBreak pos label) =
                let info = StmtInfo (map (fmap getType) scope)
                                    (fmap getStmt (breakLabels M.! label))
                                    pos
                in  (insert pos (StmtBreak info) stmtDict,scope)
            addAstStmt (AstStmtReturn pos astExpr) =
                let info = StmtInfo (map (fmap getType) scope) Nothing pos
                in  (insert pos (StmtReturn info (fmap toExpr astExpr))
                            stmtDict,
                     scope)
            addAstStmt (AstStmtSetClear pos bit astExpr) =
                (insert pos (StmtSetClear (stmtInfo pos) bit (toExpr astExpr))
                        stmtDict,
                 scope)
            addAstStmt (AstStmtAssign pos lhs rhs) =
                (insert pos
                        (StmtAssign (stmtInfo pos) (toExpr lhs)
                                    (toAliasableExpr aliasScope rhs))
                        stmtDict,
                 scope)
              where
                aliasScope | M.null breakLabels = [] | otherwise = scope
            addAstStmt (AstStmtExpr pos astExpr) =
                (insert pos (StmtExpr (stmtInfo pos) (toExpr astExpr))
                        stmtDict,
                 scope)

canAlias :: AstType -> AstType -> Bool
canAlias astType1 astType2 =
    astType1 == astType2 || any (flip canAlias astType2) fieldTypes
  where
    fieldTypes = map (\ (_,(_,_,astType)) -> astType) (astTypeFields astType1)

funcMaxAliases :: Func rtt rtf -> Int
funcMaxAliases (ImportFunc _ _) = 0
funcMaxAliases (Func _ stmt) = stmtMaxAliases stmt
  where
    stmtMaxAliases (StmtBlock _ []) = 0
    stmtMaxAliases (StmtBlock _ stmts) = maximum (map stmtMaxAliases stmts)
    stmtMaxAliases (StmtVar _ _ _ varAliases expr) =
        max varAliases (maybe 0 exprMaxAliases expr)
    stmtMaxAliases (StmtIf _ _ ifBlock elseBlock) =
        max (stmtMaxAliases ifBlock) (maybe 0 stmtMaxAliases elseBlock)
    stmtMaxAliases (StmtFor _ stmt) = stmtMaxAliases stmt
    stmtMaxAliases (StmtAssign _ _ rhs) = exprMaxAliases rhs
    stmtMaxAliases _ = 0

    exprMaxAliases (ExprVar _) = 0
    exprMaxAliases (ExprField _ _ _ expr) = exprMaxAliases expr
    exprMaxAliases (ExprFunc _ _ _ (maxAliases,_)) = maxAliases

funcType :: Func rtt rtf -> Maybe (Type rtt)
funcType (Func (FuncSig _ funcType) _) = funcType
funcType (ImportFunc (FuncSig _ funcType) _) = funcType
