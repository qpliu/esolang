module AST.Parse(
    parse
)
where

import Control.Monad(foldM,foldM_)
import Control.Monad.ST(ST,runST)
import Data.Char(isAlphaNum,isSpace)
import Data.List(groupBy,sortBy)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.STRef(STRef,modifySTRef,newSTRef,readSTRef,writeSTRef)

import AST.AST(
    Module(Library,Program),
    Routine(Routine),
    Var(Var),
    Val(Val,NewVal),
    Statement(LetEq,LetAddEdge,LetRemoveEdge,If,Call,Return,DoLoop,DoEdges,Exit),
    IfBranch(IfEq,IfEdge,IfElse),
    moduleName,moduleSubroutines,moduleProgram,moduleExterns,moduleSourceFileName,moduleSourceDirectory,
    routineName,routineArgs,routineStmts,routineExported,routineVars,routineDoEdgesCount,routineCallArgsMaxCount,routineSourceLineNumber,routineEndSourceLineNumber,
    varName,varIsCallArg,
    ifBranchStmts)

parse :: FilePath -> FilePath -> String -> Either String Module
parse filename directory src = parseModule filename directory $ filter (not . null . snd) $ map (fmap (tokenize . stripSpaces)) $ zip [1..] $ lines src
  where
    stripSpaces line = filter (not . isSpace) $ takeWhile (/= '*') line
    tokenize str = groupBy areAlphaNum str
    areAlphaNum c1 c2 = isAlphaNum c1 && isAlphaNum c2

type Lines = [(Integer,[String])]

parseModule :: FilePath -> FilePath -> Lines -> Either String Module
parseModule filename directory lines = do
    (uses,lines) <- parseUses lines
    (subroutines,externs,lines) <- parseSubroutines lines
    (maybeExports,lines) <- parseLibrary lines
    maybe (parseProgram filename directory lines uses subroutines externs)
          (makeLibrary filename directory lines uses subroutines externs) maybeExports

type Externs = [(Integer,(String,String))]

makeLibrary :: FilePath -> FilePath -> Lines -> Set.Set String -> Map.Map String Routine -> Externs -> (String,Map.Map String Integer) -> Either String Module
makeLibrary filename directory ((lineNumber,_):_) uses subroutines externs (name,exports) = do
    parseError lineNumber "TRAILING JUNK"
makeLibrary filename directory [] uses subroutines externs (name,exports) = do
    mapM_ checkExport $ Map.toList exports
    externs <- foldM checkExtern Set.empty externs
    return Library {
        moduleName = name,
        moduleSubroutines = (map markExport . Map.elems) subroutines,
        moduleExterns = Set.toList externs,
        moduleSourceFileName = filename,
        moduleSourceDirectory = directory
        }
  where
    checkExport (export,lineNumber)
      | Map.member export subroutines = return ()
      | otherwise = parseError lineNumber ("UNDEFINED SUBROUTINE: " ++ export)
    checkExtern externs (lineNumber,extern@(externMod,_))
      | Set.member externMod uses = return $ Set.insert extern externs
      | otherwise = parseError lineNumber ("LIBRARY NOT DECLARED USED: " ++ externMod)
    markExport routine =
        routine { routineExported = Map.member (routineName routine) exports }

makeProgram :: FilePath -> FilePath -> Lines -> Set.Set String -> Map.Map String Routine -> Routine -> Externs -> Either String Module
makeProgram filename directory ((lineNumber,_):_) uses subroutines program externs = do
    parseError lineNumber "TRAILING JUNK"
makeProgram filename directory [] uses subroutines program externs = do
    externs <- foldM checkExtern Set.empty externs
    mapM_ checkReturn $ routineStmts program
    return Program {
        moduleName = routineName program,
        moduleSubroutines = Map.elems subroutines,
        moduleProgram = program,
        moduleExterns = Set.toList externs,
        moduleSourceFileName = filename,
        moduleSourceDirectory = directory
        }
  where
    checkExtern externs (lineNumber,extern@(externMod,_))
      | Set.member externMod uses = return $ Set.insert extern externs
      | otherwise = parseError lineNumber ("LIBRARY NOT DECLARED USED: " ++ externMod)
    checkReturn (If ifBranches _ _) = mapM_ checkReturn $ concatMap ifBranchStmts ifBranches
    checkReturn (Return lineNumber) = parseError lineNumber "INVALID RETURN"
    checkReturn (DoLoop _ _ stmts _ _) = mapM_ checkReturn stmts
    checkReturn (DoEdges _ _ _ stmts _ _) = mapM_ checkReturn stmts
    checkReturn _ = return ()

parseUses :: Lines -> Either String (Set.Set String,Lines)
parseUses lines = parseUse lines Set.empty
  where
    parseUse [] _ = eofError
    parseUse ((lineNumber,['U':'S':'E':use]):lines) uses = do
        isIdent lineNumber use
        parseUse lines (Set.insert use uses)
    parseUse lines uses = return (uses,lines)

parseLibrary :: Lines -> Either String (Maybe (String,Map.Map String Integer),Lines)
parseLibrary [] = eofError
parseLibrary ((lineNumber,['L':'I':'B':'R':'A':'R':'Y':library]):lines) = do
    isIdent lineNumber library
    (exports,lines) <- parseLibraryExports lines Map.empty
    (_,lines) <- nextLineIs lines $ "END" ++ library
    return (Just (library,exports),lines)
  where
    parseLibraryExports [] _ = eofError
    parseLibraryExports ((lineNumber,['S':'U':'B':'R':'O':'U':'T':'I':'N':'E':export]):lines) exports
      | Map.member export exports = parseError lineNumber $ "DUPLICATE SUBROUTINE: " ++ export
      | otherwise = do        
        isIdent lineNumber export
        parseLibraryExports lines (Map.insert export lineNumber exports)
    parseLibraryExports lines exports = return (exports,lines)
parseLibrary lines = return (Nothing,lines)
    
parseProgram :: FilePath -> FilePath -> Lines -> Set.Set String -> Map.Map String Routine -> Externs -> Either String Module
parseProgram filename directory [] uses subroutines externs = eofError
parseProgram filename directory ((lineNumber,['P':'R':'O':'G':'R':'A':'M':name]):lines) uses subroutines externs = do
    isIdent lineNumber name
    (program,newExterns,lines) <- parseRoutineBody lines name [] lineNumber
    makeProgram filename directory lines uses subroutines program (externs ++ newExterns)
parseProgram filename directory ((lineNumber,line):_) uses subroutines externs = syntaxError lineNumber

parseSubroutines :: Lines -> Either String (Map.Map String Routine,Externs,Lines)
parseSubroutines lines = parseSubs lines Map.empty []
  where
    parseSubs :: Lines -> Map.Map String Routine -> Externs -> Either String (Map.Map String Routine,Externs,Lines)
    parseSubs [] _ _ = eofError
    parseSubs ((lineNumber,('S':'U':'B':'R':'O':'U':'T':'I':'N':'E':name):"(":args):lines) subs externs
      | Map.member name subs =
            parseError lineNumber $ "DUPLCATE SUBROUTINE: " ++ name
      | otherwise = do
            isIdent lineNumber name
            args <- parseCallArgs lineNumber args
            foldM_ checkArg Set.empty args
            (routine,newExterns,lines) <- parseRoutineBody lines name args lineNumber
            parseSubs lines (Map.insert name routine subs) (newExterns ++ externs)
      where
        checkArg args arg
          | Set.member arg args = parseError lineNumber $ "DUPLICATE PARAMETER: " ++ arg
          | otherwise = do
                isIdent lineNumber arg
                return $ Set.insert arg args
    parseSubs lines subs externs = return (subs,externs,lines)

parseRoutineBody :: Lines -> String -> [String] -> Integer -> Either String (Routine,Externs,Lines)
parseRoutineBody lines name args lineNumber = runST $ do
    varMapRef <- newSTRef Map.empty
    doIndicesRef <- newSTRef 0
    doEdgesIndicesRef <- newSTRef 0
    doStackRef <- newSTRef []
    callArgsMaxCountRef <- newSTRef 0
    externsRef <- newSTRef []
    stmtsLines <- parseRoutineStmts lines varMapRef doIndicesRef doEdgesIndicesRef doStackRef callArgsMaxCountRef externsRef args
    finalVarMap <- readSTRef varMapRef
    finalDoEdgesCount <- readSTRef doEdgesIndicesRef
    finalCallArgsMaxCount <- readSTRef callArgsMaxCountRef
    finalExterns <- readSTRef externsRef
    return $ do
        (stmts,lines) <- stmtsLines
        (endLineNumber,lines) <- nextLineIs lines ("END" ++ name)
        let vars = sortBy compareVars $ Map.elems finalVarMap
        return (Routine {
            routineName = name,
            routineArgs = take (length args) vars,
            routineStmts = stmts,
            routineExported = False,
            routineVars = vars,
            routineDoEdgesCount = finalDoEdgesCount,
            routineCallArgsMaxCount = finalCallArgsMaxCount,
            routineSourceLineNumber = lineNumber,
            routineEndSourceLineNumber = endLineNumber
            },finalExterns,lines)
  where
    parseRoutineStmts :: Lines -> STRef a (Map.Map String Var) -> STRef a Integer -> STRef a Integer -> STRef a [(String,Integer)] -> STRef a Integer -> STRef a Externs -> [String] -> ST a (Either String ([Statement],Lines))
    parseRoutineStmts lines varMapRef doIndicesRef doEdgesIndicesRef doStackRef callArgsMaxCountRef externsRef args = do
        mapM_ (getVar lineNumber True) args
        parseStmts lines
      where
        -- getVar :: Integer -> Bool -> String -> ST a (Either String Var)
        getVar lineNumber isCallArg name = do
            varMap <- readSTRef varMapRef
            case Map.lookup name varMap of
                Just var -> return $ return var
                Nothing -> do
                    let var = Var name (fromIntegral $ Map.size varMap) isCallArg
                    writeSTRef varMapRef (Map.insert name var varMap)
                    return $ do
                        isIdent lineNumber name
                        return var

        -- getVal :: Integer -> String -> ST a (Either String Val)
        getVal lineNumber "0" = return $ return NewVal
        getVal lineNumber name = fmap (fmap Val) $ getVar lineNumber False name

        -- parseStmts :: Lines -> ST a (Either String ([Statement],Lines))
        parseStmts [] = return eofError
        parseStmts lines = do
            maybeStmtLines <- parseStmt lines
            let (maybeStmt,lines) = either (const (Nothing,[])) id maybeStmtLines
            stmtsLines <- maybe (return $ return ([],[])) (const $ parseStmts lines) maybeStmt
            return $ do
                (maybeStmt,lines) <- maybeStmtLines
                maybe (return ([],lines)) (addStmt stmtsLines) maybeStmt
          where
            addStmt stmtsLines stmt = do
                (stmts,lines) <- stmtsLines
                return (stmt:stmts,lines)

        -- parseStmt :: Lines -> ST a (Either String (Maybe Statement,Lines))
        parseStmt [] = return eofError
        parseStmt ((lineNumber,['L':'E':'T':arg0,"=",arg1]):lines) = do
            var <- getVar lineNumber False arg0
            val <- getVal lineNumber arg1
            return $ do
                var <- var
                val <- val
                return (Just $ LetEq var val lineNumber,lines)
        parseStmt ((lineNumber,['L':'E':'T':arg0,">",arg1]):lines) = do
            var <- getVar lineNumber False arg0
            val <- getVal lineNumber arg1
            return $ do
                var <- var
                val <- val
                return (Just $ LetAddEdge var val lineNumber,lines)
        parseStmt ((lineNumber,['L':'E':'T':arg0,"<",arg1]):lines) = do
            var0 <- getVar lineNumber False arg0
            var1 <- getVar lineNumber False arg1
            return $ do
                var0 <- var0
                var1 <- var1
                return (Just $ LetRemoveEdge (var0,var1) lineNumber,lines)
        parseStmt ((lineNumber,token@('I':'F':_):tokens):lines) = do
            ifBranchesLines <- parseIfBranches ((lineNumber,("ELSE"++token):tokens):lines)
            return $ do
                (ifBranches,endLineNumber,lines) <- ifBranchesLines
                return (Just $ If ifBranches lineNumber endLineNumber,lines)
        parseStmt ((lineNumber,('C':'A':'L':'L':mod):".":rout:"(":args):lines) = do
            modifySTRef externsRef ((lineNumber,(mod,rout)):)
            args <- return $ parseCallArgs lineNumber args
            modifySTRef callArgsMaxCountRef $ max (either (const 0) (fromIntegral . length) args)
            args <- mapM (getVal lineNumber) $ either (const []) id args
            return $ do
                isIdent lineNumber mod
                isIdent lineNumber rout
                args <- sequence args
                return $ (Just $ Call (Just mod,rout) args lineNumber,lines)
        parseStmt ((lineNumber,('C':'A':'L':'L':rout):"(":args):lines) = do
            args <- return $ parseCallArgs lineNumber args
            modifySTRef callArgsMaxCountRef $ max (either (const 0) (fromIntegral . length) args)
            args <- mapM (getVal lineNumber) $ either (const []) id args
            return $ do
                isIdent lineNumber rout
                args <- sequence args
                return $ (Just $ Call (Nothing,rout) args lineNumber,lines)
        parseStmt ((lineNumber,["RETURN"]):lines) = do
            return $ return (Just $ Return lineNumber,lines)
        parseStmt ((lineNumber,['D':'O':arg]):lines) = do
            doIndex <- readSTRef doIndicesRef
            modifySTRef doIndicesRef (+1)
            modifySTRef doStackRef ((arg,doIndex):)
            arg <- getVar lineNumber False arg
            stmtsLines <- parseStmts lines
            modifySTRef doStackRef tail
            return $ do
                arg <- arg
                (stmts,lines) <- stmtsLines
                (endLineNumber,lines) <- nextLineIs lines "ENDDO"
                return $ (Just $ DoLoop arg doIndex stmts lineNumber endLineNumber,lines)
        parseStmt ((lineNumber,['D':'O':arg0,"<",arg1]):lines) = do
            doIndex <- readSTRef doIndicesRef
            modifySTRef doIndicesRef (+1)
            modifySTRef doStackRef ((arg0,doIndex):)
            doEdgesIndex <- readSTRef doEdgesIndicesRef
            modifySTRef doEdgesIndicesRef (+1)
            arg0 <- getVar lineNumber False arg0
            arg1 <- getVar lineNumber False arg1
            stmtsLines <- parseStmts lines
            modifySTRef doStackRef tail
            return $ do
                arg0 <- arg0
                arg1 <- arg1
                (stmts,lines) <- stmtsLines
                (endLineNumber,lines) <- nextLineIs lines "ENDDO"
                return $ (Just $ DoEdges (arg0,arg1) doIndex doEdgesIndex stmts lineNumber endLineNumber,lines)
        parseStmt ((lineNumber,['E':'X':'I':'T':arg]):lines) = do
            doStack <- readSTRef doStackRef
            arg <- getVar lineNumber False arg
            return $ do
                arg <- arg
                maybe exitError (exitStmt arg) $ lookup (varName arg) doStack
          where
            exitError = parseError lineNumber $ "INVALID EXIT LABEL: " ++ arg
            exitStmt arg doIndex = return (Just $ Exit arg doIndex lineNumber,lines)
        parseStmt lines = return $ return (Nothing,lines)

        -- parseIfBranches :: Lines -> ST a (Either String ([IfBranch],Integer,Lines))
        parseIfBranches [] = return eofError
        parseIfBranches ((lineNumber,["ENDIF"]):lines) =
            return $ return ([],lineNumber,lines)
        parseIfBranches ((lineNumber,["ELSE"]):lines) = do
            stmtsLines <- parseStmts lines
            return $ do
                (stmts,lines) <- stmtsLines
                (endLineNumber,lines) <- nextLineIs lines "ENDIF"
                return ([IfElse stmts lineNumber],endLineNumber,lines)
        parseIfBranches ((lineNumber,['E':'L':'S':'E':'I':'F':arg0,"=",arg1]):lines) = do
            arg0 <- getVar lineNumber False arg0
            arg1 <- getVar lineNumber False arg1
            stmtsLines <- parseStmts lines
            lines <- return $ either (const []) snd stmtsLines
            ifBranchesLines <- parseIfBranches lines
            return $ do
                arg0 <- arg0
                arg1 <- arg1
                (stmts,_) <- stmtsLines
                (ifBranches,endLineNumber,lines) <- ifBranchesLines
                return (IfEq (arg0,arg1) stmts lineNumber:ifBranches,endLineNumber,lines)
        parseIfBranches ((lineNumber,['E':'L':'S':'E':'I':'F':arg0,">",arg1]):lines) = do
            arg0 <- getVar lineNumber False arg0
            arg1 <- getVar lineNumber False arg1
            stmtsLines <- parseStmts lines
            lines <- return $ either (const []) snd stmtsLines
            ifBranchesLines <- parseIfBranches lines
            return $ do
                arg0 <- arg0
                arg1 <- arg1
                (stmts,_) <- stmtsLines
                (ifBranches,endLineNumber,lines) <- ifBranchesLines
                return (IfEdge (arg0,arg1) stmts lineNumber:ifBranches,endLineNumber,lines)
        parseIfBranches ((lineNumber,_):_) = return $ syntaxError lineNumber
    compareVars (Var _ varIndex1 _) (Var _ varIndex2 _) =
        compare varIndex1 varIndex2

parseCallArgs :: Integer -> [String] -> Either String [String]
parseCallArgs lineNumber [")"] = return []
parseCallArgs lineNumber [arg,")"] = return [arg]
parseCallArgs lineNumber (arg:",":args) = do
    args <- parseCallArgs lineNumber args
    return $ arg:args
parseCallArgs lineNumber _ = syntaxError lineNumber

nextLineIs :: Lines -> String -> Either String (Integer,Lines)
nextLineIs [] str = eofError
nextLineIs ((lineNumber,line):lines) str
  | line == [str] = return (lineNumber,lines)
  | otherwise = syntaxError lineNumber

isIdent :: Integer -> String -> Either String ()
isIdent lineNumber str
  | str /= "0" && str /= "" && all isAlphaNum str = return ()
  | otherwise = syntaxError lineNumber

eofError :: Either String a
eofError = Left "UNEXPECTED EOF"

parseError :: Integer -> String -> Either String a
parseError lineNumber message = Left (show lineNumber ++ ": " ++ message)

syntaxError :: Integer -> Either String a
syntaxError lineNumber = parseError lineNumber "SYNTAX ERROR"
