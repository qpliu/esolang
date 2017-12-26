module AST.Parse(
    parse
)
where

import Control.Monad.ST(ST,runST)
import Data.Char(isAlphaNum,isSpace)
import Data.List(groupBy)
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
    moduleName,moduleSubroutines,moduleProgram,moduleExterns,
    routineName,routineArgs,routineStmts,routineExported,routineVarCount,routineDoEdgesCount,routineCallArgsMaxCount,
    varName,varIndex,
    stmtVar,stmtVal,stmtVars,stmtIfBranches,stmtCallTarget,stmtCallArgs,stmtDoIndex,stmtStmts,stmtDoEdgesIndex,
    ifBranchVars,ifBranchStmts)

parse :: String -> Either String Module
parse src = (parseModule . filter (/= "") . map stripSpaces . lines) src
  where stripSpaces line = filter (not . isSpace) $ takeWhile (/= '*') line

parseModule :: [String] -> Either String Module
parseModule lines = do
    (uses,lines) <- parseUses lines
    (subroutines,externs,lines) <- parseSubroutines lines
    (maybeExports,lines) <- parseLibrary lines
    maybe (parseProgram lines uses subroutines externs)
          (makeLibrary lines uses subroutines externs) maybeExports

makeLibrary :: [String] -> Set.Set String -> Map.Map String Routine -> Set.Set (String,String) -> (String,Set.Set String) -> Either String Module
makeLibrary lines uses subroutines externs (name,exports)
  | (not . null) lines =
        Left "TRAILING JUNK"
  | (not . all validExport . Set.toList) exports =
        Left "UNDEFINED LIBRARY SUBROUTINE"
  | unusedExtern uses externs =
        Left "CALL INTO UNUSED MODULE"
  | otherwise =
        return Library {
            moduleName = name,
            moduleSubroutines = (map markExport . Map.elems) subroutines,
            moduleExterns = Set.toList externs
            }
  where
    validExport export = Map.member export subroutines
    markExport routine =
        routine { routineExported = Set.member (routineName routine) exports }

makeProgram :: [String] -> Set.Set String -> Map.Map String Routine -> Routine -> Set.Set (String,String) -> Either String Module
makeProgram lines uses subroutines program externs
  | (not . null) lines =
        Left "TRAILING JUNK"
  | unusedExtern uses externs =
        Left "CALL INTO UNUSED MODULE"
  | hasReturn program =
        Left "INVALID RETURN"
  | otherwise =
        return Program {
            moduleName = routineName program,
            moduleSubroutines = Map.elems subroutines,
            moduleProgram = program,
            moduleExterns = Set.toList externs
            }

parseUses :: [String] -> Either String (Set.Set String,[String])
parseUses lines = parseUse Set.empty lines
  where
    parseUse uses lines
      | null lines =
            Left "UNEXPECTED EOF"
      | take 3 (head lines) /= "USE" =
            return (uses,lines)
      | Set.member use uses =
            Left ("DUPLICATE USE " ++ use)
      | otherwise = do
            isIdent use
            parseUse (Set.insert use uses) (tail lines)
      where
        use = (drop 3 . head) lines

parseLibrary :: [String] -> Either String (Maybe (String,Set.Set String),[String])
parseLibrary lines
  | null lines =
        Left "UNEXPECTED EOF"
  | take 7 (head lines) /= "LIBRARY" =
        return (Nothing,lines)
  | otherwise = do
        isIdent library
        parseLibraryExports Set.empty (tail lines)
  where
    library = (drop 7 . head) lines
    parseLibraryExports exports lines
      | null lines =
            Left "UNEXPECTED EOF"
      | "END" ++ library == head lines =
            return (Just (library,exports),tail lines)
      | take 10 (head lines) /= "SUBROUTINE" =
            Left "SYNTAX ERROR"
      | Set.member export exports =
            Left ("DUPLICATE SUBROUTINE " ++ export)
      | otherwise = do
            isIdent export
            parseLibraryExports (Set.insert export exports) (tail lines)
      where
        export = (drop 10 . head) lines
    
parseProgram :: [String] -> Set.Set String -> Map.Map String Routine -> Set.Set (String,String) -> Either String Module
parseProgram lines uses subroutines externs
  | null lines =
        Left "UNEXPECTED EOF"
  | take 7 (head lines) /= "PROGRAM" =
        Left "SYNTAX ERROR"
  | otherwise = do
        isIdent programName
        (program,newExterns,lines) <- parseRoutineBody (tail lines) programName []
        makeProgram lines uses subroutines program (Set.union newExterns externs)
  where
    programName = (drop 7 . head) lines

parseSubroutines :: [String] -> Either String (Map.Map String Routine,Set.Set (String,String),[String])
parseSubroutines lines = parseSubs Map.empty Set.empty lines
  where
    parseSubs subs externs lines
      | null lines =
            Left "UNEXPECTED EOF"
      | take 10 (head lines) /= "SUBROUTINE" =
            return (subs,externs,lines)
      | otherwise = do
            let tokens = (tokenize . drop 10 . head) lines
            args <- parseCallArgs (drop 1 tokens)
            let name = head tokens
            isIdent name
            if Map.member name subs
              then Left ("DUPLICATE SUBROUTINE NAME " ++ name)
              else return ()
            (routine,newExterns,lines) <- parseRoutineBody (tail lines) name args
            parseSubs (Map.insert name routine subs) (Set.union externs newExterns) (tail lines)

parseRoutineBody :: [String] -> String -> [String] -> Either String (Routine,Set.Set (String,String),[String])
parseRoutineBody lines name args = runST $ do
    varMapRef <- newSTRef Map.empty
    doIndicesRef <- newSTRef 0
    doEdgesIndicesRef <- newSTRef 0
    doStackRef <- newSTRef []
    callArgsMaxCountRef <- newSTRef 0
    externsRef <- newSTRef Set.empty
    addedArgs <- mapM (addArg varMapRef) args
    stmtsLines <- parseRoutineStmts lines varMapRef doIndicesRef doEdgesIndicesRef doStackRef callArgsMaxCountRef externsRef
    finalVarMap <- readSTRef varMapRef
    finalDoEdgesCount <- readSTRef doEdgesIndicesRef
    finalCallArgsMaxCount <- readSTRef callArgsMaxCountRef
    finalExterns <- readSTRef externsRef
    return $ do
        argVars <- sequence addedArgs
        (stmts,lines) <- stmtsLines
        lines <- nextLineIs lines ("END" ++ name)
        return (Routine {
            routineName = name,
            routineArgs = argVars,
            routineStmts = stmts,
            routineExported = False,
            routineVarCount = (fromIntegral . Map.size) finalVarMap,
            routineDoEdgesCount = finalDoEdgesCount,
            routineCallArgsMaxCount = finalCallArgsMaxCount
            },finalExterns,lines)
  where
    addArg :: STRef a (Map.Map String Var) -> String -> ST a (Either String Var)
    addArg varMapRef arg = do
        varMap <- readSTRef varMapRef
        if Map.member arg varMap
          then return (Left ("DUPLICATE ARGUMENT " ++ arg))
          else do
            let var = Var arg (fromIntegral $ Map.size varMap)
            writeSTRef varMapRef (Map.insert arg var varMap)
            return $ do
                isIdent arg
                return var
    parseRoutineStmts lines varMapRef doIndicesRef doEdgesIndicesRef doStackRef callArgsMaxCountRef externsRef = 
        parseStmts lines
      where
        getVar varName = do
            varMap <- readSTRef varMapRef
            case Map.lookup varName varMap of
                Just var -> (return . return) var
                Nothing -> do
                    let var = Var varName (fromIntegral $ Map.size varMap)
                    writeSTRef varMapRef (Map.insert varName var varMap)
                    return $ do
                        isIdent varName
                        return var

        getVal valName
          | valName == "0" =
                (return . return) NewVal
          | otherwise = do
                var <- getVar valName
                return (fmap Val var)

        -- parseStmts :: [String] -> ST a (Either String ([Statement],[String]))
        parseStmts lines = do
            maybeStmtLines <- parseStmt lines
            either (return . Left) parseMoreStmts maybeStmtLines

        -- parseMoreStmts :: (Maybe Statement,[String]) -> ST a (Either String ([Statement],[String]))
        parseMoreStmts (Nothing,lines) =
            (return . return) ([],lines)
        parseMoreStmts (Just stmt,lines) = do
            stmtsLines <- parseStmts lines
            either (return . Left) (consStmt stmt) stmtsLines

        consStmt :: Statement -> ([Statement],[String]) -> ST a (Either String ([Statement],[String]))
        consStmt stmt (stmts,lines) =
            (return . return) (stmt:stmts,lines)

        -- parseStmt :: [String] -> ST a (Either String (Maybe Statement,[String]))
        parseStmt lines
          | null lines =
                (return . Left) "UNEXPECTED EOF"
          | (take 3 . head) lines == "LET" = do
                stmt <- parseLet ((tokenize . drop 3 . head) lines)
                return $ do
                    stmt <- stmt
                    return (Just stmt,tail lines)
          | (take 2 . head) lines == "IF" = do
                stmtLines <- parseIf (tail lines) (head lines)
                return $ do
                    (stmt,lines) <- stmtLines
                    return (Just stmt,lines)
          | (take 4 . head) lines == "CALL" = do
                stmt <- parseCall ((tokenize . drop 4 . head) lines)
                return $ do
                    stmt <- stmt
                    return (Just stmt,tail lines)
          | head lines == "RETURN" = do
                (return . return) (Just Return,tail lines)
          | (take 2 . head) lines == "DO" = do
                stmtLines <- parseDo (tail lines) ((tokenize . drop 2 . head) lines)
                return $ do
                    (stmt,lines) <- stmtLines
                    return (Just stmt,lines)
          | (take 4 . head) lines == "EXIT" = do
                stmt <- parseExit ((drop 4 . head) lines)
                return $ do
                    stmt <- stmt
                    return (Just stmt,lines)
          | otherwise = do
                (return . return) (Nothing,lines)

        -- parseLet :: [String] -> ST a (Either String Statement)
        parseLet [arg0,"=",arg1] = do
            var <- getVar arg0
            val <- getVal arg1
            return $ do
                isIdent arg0
                isVal arg1
                var <- var
                val <- val
                return (LetEq var val)
        parseLet [arg0,">",arg1] = do
            var <- getVar arg0
            val <- getVal arg1
            return $ do
                isIdent arg0
                isVal arg1
                var <- var
                val <- val
                return (LetAddEdge var val)
        parseLet [arg0,"<",arg1] = do
            var0 <- getVar arg0
            var1 <- getVar arg1
            return $ do
                isIdent arg0
                isIdent arg1
                var0 <- var0
                var1 <- var1
                return (LetRemoveEdge (var0,var1))
        parseLet _ =
            (return . Left) "SYNTAX ERROR"

        parseIf :: [String] -> String -> ST a (Either String (Statement,[String]))
        parseIf lines line = do
            ifBranchesLines <- parseIfBranches (("ELSE" ++ line):lines)
            return $ do
                (ifBranches,lines) <- ifBranchesLines
                return (If ifBranches,lines)

        -- parseCall :: [String] -> ST a (Either String Statement)
        parseCall (mod:".":rout:tokens@("(":_)) = do
            modifySTRef externsRef (Set.insert (mod,rout))
            args <- (return . parseCallArgs) tokens
            vals <- either (return . Left) (fmap sequence . mapM getVal) args
            -- getVal :: String -> ST a (Either String Val)
            -- mapM getVal :: [String] -> ST a [Either String Val]
            -- fmap sequence :: ST a [Either String Val] -> ST a (Either String [Val])
            return $ do
                isIdent mod
                isIdent rout
                args <- args
                mapM_ isVal args
                vals <- vals
                return $ Call (Just mod,rout) vals
        parseCall (rout:tokens@("(":_)) = do
            args <- (return . parseCallArgs) tokens
            vals <- either (return . Left) (fmap sequence . mapM getVal) args
            return $ do
                isIdent rout
                args <- args
                mapM_ isVal args
                vals <- vals
                return $ Call (Nothing,rout) vals
        parseCall _ =
            (return . Left) "SYNTAX ERROR"

        -- parseDo :: [String] -> [String] -> ST a (Either String (Statement,[String]))
        parseDo lines [arg] = do
            doIndex <- readSTRef doIndicesRef
            modifySTRef doIndicesRef (+1)
            modifySTRef doStackRef ((arg,doIndex):)
            var <- getVar arg
            stmtsLines <- parseStmts (tail lines)
            modifySTRef doStackRef tail
            return $ do
                isIdent arg
                var <- var
                (stmts,lines) <- stmtsLines
                lines <- nextLineIs lines "ENDDO"
                return (DoLoop var doIndex stmts,lines)
        parseDo lines [arg0,"<",arg1] = do
            doIndex <- readSTRef doIndicesRef
            modifySTRef doIndicesRef (+1)
            modifySTRef doStackRef ((arg0,doIndex):)
            doEdgesIndex <- readSTRef doEdgesIndicesRef
            modifySTRef doEdgesIndicesRef (+1)
            var0 <- getVar arg0
            var1 <- getVar arg1
            stmtsLines <- parseStmts (tail lines)
            modifySTRef doStackRef tail
            return $ do
                isIdent arg0
                isIdent arg1
                var0 <- var0
                var1 <- var1
                (stmts,lines) <- stmtsLines
                lines <- nextLineIs lines "ENDDO"
                return (DoEdges (var0,var1) doIndex doEdgesIndex stmts,lines)
        parseDo _ _ =
            (return . Left) "SYNTAX ERROR"

        -- parseExit :: String -> ST a (Either String Statement)
        parseExit arg = do
            doStack <- readSTRef doStackRef
            var <- getVar arg
            return $ do
                var <- var
                maybe (Left "SYNTAX ERROR") (return . Exit var) (lookup arg doStack)

        parseIfBranches :: [String] -> ST a (Either String ([IfBranch],[String]))
        parseIfBranches lines = undefined

parseCallArgs :: [String] -> Either String [String]
parseCallArgs tokens
  | null tokens || head tokens /= "(" =
        Left "SYNTAX ERROR"
  | tokens == ["(",")"] =
        return []
  | otherwise =
        (fmap reverse . parseArgs [] . tail) tokens
  where
    parseArgs revVars (name:[")"]) = return (name:revVars)
    parseArgs revVars (name:",":rest) = parseArgs (name:revVars) rest
    parseArgs _ _ = Left "SYNTAX ERROR"

nextLineIs :: [String] -> String -> Either String [String]
nextLineIs lines str
  | null lines =
        Left "UNEXPECTED EOF"
  | head lines /= str =
        Left "SYNTAX ERROR"
  | otherwise =
        (return . tail) lines

isIdent :: String -> Either String ()
isIdent str
  | str /= "0" && str /= "" && all isAlphaNum str =
        return ()
  | otherwise =
        Left "SYNTAX ERROR"

isVal :: String -> Either String ()
isVal str
  | str /= "" && all isAlphaNum str =
        return ()
  | otherwise =
        Left "SYNTAX ERROR"

unusedExtern :: Set.Set String -> Set.Set (String,String) -> Bool
unusedExtern uses externs =
    (not . all (flip Set.member uses . fst) . Set.toList) externs

hasReturn :: Routine -> Bool
hasReturn routine = (any hasRet . routineStmts) routine
  where
    hasRet (If ifBranches) = any (any hasRet . ifBranchStmts) ifBranches
    hasRet Return = True
    hasRet (DoLoop _ _ stmts) = any hasRet stmts
    hasRet (DoEdges _ _ _ stmts) = any hasRet stmts
    hasRet _ = False

tokenize :: String -> [String]
tokenize str = groupBy areAlphaNum str
  where
    areAlphaNum c1 c2 = isAlphaNum c1 && isAlphaNum c2
