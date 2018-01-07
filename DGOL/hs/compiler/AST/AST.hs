module AST.AST(
    Module(Library,Program),
    Routine(Routine),
    Var(Var),
    Val(Val,NewVal),
    Statement(LetEq,LetAddEdge,LetRemoveEdge,If,Call,Return,DoLoop,DoEdges,Exit),
    IfBranch(IfEq,IfEdge,IfElse),
    moduleName,moduleSubroutines,moduleProgram,moduleExterns,moduleSourceFileName,moduleSourceDirectory,
    routineName,routineArgs,routineStmts,routineExported,routineVars,routineDoEdgesCount,routineCallArgsMaxCount,routineSourceLineNumber,routineEndSourceLineNumber,
    varName,varIndex,varIsCallArg,
    stmtVar,stmtVal,stmtVars,stmtIfBranches,stmtCallTarget,stmtCallArgs,stmtDoIndex,stmtStmts,stmtDoEdgesIndex,stmtSourceLineNumber,stmtEndSourceLineNumber,
    ifBranchVars,ifBranchStmts,ifBranchSourceLineNumber
)
where

data Module =
    Library {
        moduleName :: String,
        moduleSubroutines :: [Routine],
        moduleExterns :: [(String,String)],
        moduleSourceFileName :: FilePath,
        moduleSourceDirectory :: FilePath
        }
  | Program {
        moduleName :: String,
        moduleSubroutines :: [Routine],
        moduleProgram :: Routine,
        moduleExterns :: [(String,String)],
        moduleSourceFileName :: FilePath,
        moduleSourceDirectory :: FilePath
        }
    deriving Show

data Routine = Routine {
    routineName :: String,
    routineArgs :: [Var],
    routineStmts :: [Statement],
    routineExported :: Bool,
    routineVars :: [Var],
    routineDoEdgesCount :: Integer,
    routineCallArgsMaxCount :: Integer,
    routineSourceLineNumber :: Integer,
    routineEndSourceLineNumber :: Integer
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
        stmtVal :: Val,
        stmtSourceLineNumber :: Integer
        }
  | LetAddEdge {
        stmtVar :: Var,
        stmtVal :: Val,
        stmtSourceLineNumber :: Integer
        }
  | LetRemoveEdge {
        stmtVars :: (Var,Var),
        stmtSourceLineNumber :: Integer
        }
  | If {
        stmtIfBranches :: [IfBranch],
        stmtSourceLineNumber :: Integer,
        stmtEndSourceLineNumber :: Integer
        }
  | Call {
        stmtCallTarget :: (Maybe String,String),
        stmtCallArgs :: [Val],
        stmtSourceLineNumber :: Integer
        }
  | Return {
        stmtSourceLineNumber :: Integer
        }
  | DoLoop {
        stmtVar :: Var,
        stmtDoIndex :: Integer,
        stmtStmts :: [Statement],
        stmtSourceLineNumber :: Integer,
        stmtEndSourceLineNumber :: Integer
        }
  | DoEdges {
        stmtVars :: (Var,Var),
        stmtDoIndex :: Integer,
        stmtDoEdgesIndex :: Integer,
        stmtStmts :: [Statement],
        stmtSourceLineNumber :: Integer,
        stmtEndSourceLineNumber :: Integer
        }
  | Exit {
        stmtVar :: Var,
        stmtDoIndex :: Integer,
        stmtSourceLineNumber :: Integer
        }
    deriving Show

data IfBranch =
    IfEq {
        ifBranchVars :: (Var,Var),
        ifBranchStmts :: [Statement],
        ifBranchSourceLineNumber :: Integer
        }
  | IfEdge {
        ifBranchVars :: (Var,Var),
        ifBranchStmts :: [Statement],
        ifBranchSourceLineNumber :: Integer
        }
  | IfElse {
        ifBranchStmts :: [Statement],
        ifBranchSourceLineNumber :: Integer
        }
    deriving Show
