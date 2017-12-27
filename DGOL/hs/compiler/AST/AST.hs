module AST.AST(
    Module(Library,Program),
    Routine(Routine),
    Var(Var),
    Val(Val,NewVal),
    Statement(LetEq,LetAddEdge,LetRemoveEdge,If,Call,Return,DoLoop,DoEdges,Exit),
    IfBranch(IfEq,IfEdge,IfElse),
    moduleName,moduleSubroutines,moduleProgram,moduleExterns,
    routineName,routineArgs,routineStmts,routineExported,routineVarCount,routineDoEdgesCount,routineCallArgsMaxCount,
    varName,varIndex,varIsCallArg,
    stmtVar,stmtVal,stmtVars,stmtIfBranches,stmtCallTarget,stmtCallArgs,stmtDoIndex,stmtStmts,stmtDoEdgesIndex,
    ifBranchVars,ifBranchStmts
)
where

data Module =
    Library {
        moduleName :: String,
        moduleSubroutines :: [Routine],
        moduleExterns :: [(String,String)]
        }
  | Program {
        moduleName :: String,
        moduleSubroutines :: [Routine],
        moduleProgram :: Routine,
        moduleExterns :: [(String,String)]
        }
    deriving Show

data Routine = Routine {
    routineName :: String,
    routineArgs :: [Var],
    routineStmts :: [Statement],
    routineExported :: Bool,
    routineVarCount :: Integer,
    routineDoEdgesCount :: Integer,
    routineCallArgsMaxCount :: Integer
    }
    deriving Show

data Var = Var {
    varName :: String,
    varIndex :: Integer,
    varIsCallArg :: Bool
    }
    deriving Show

data Val = Val Var | NewVal
    deriving Show

data Statement =
    LetEq {
        stmtVar :: Var,
        stmtVal :: Val
        }
  | LetAddEdge {
        stmtVar :: Var,
        stmtVal :: Val
        }
  | LetRemoveEdge {
        stmtVars :: (Var,Var)
        }
  | If {
        stmtIfBranches :: [IfBranch]
        }
  | Call {
        stmtCallTarget :: (Maybe String,String),
        stmtCallArgs :: [Val]
        }
  | Return
  | DoLoop {
        stmtVar :: Var,
        stmtDoIndex :: Integer,
        stmtStmts :: [Statement]
        }
  | DoEdges {
        stmtVars :: (Var,Var),
        stmtDoIndex :: Integer,
        stmtDoEdgesIndex :: Integer,
        stmtStmts :: [Statement]
        }
  | Exit {
        stmtVar :: Var,
        stmtDoIndex :: Integer
        }
    deriving Show

data IfBranch =
    IfEq {
        ifBranchVars :: (Var,Var),
        ifBranchStmts :: [Statement]
        }
  | IfEdge {
        ifBranchVars :: (Var,Var),
        ifBranchStmts :: [Statement]
        }
  | IfElse {
        ifBranchStmts :: [Statement]
        }
    deriving Show
