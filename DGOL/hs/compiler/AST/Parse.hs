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
            ident use
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
        ident library
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
            ident export
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
        ident programName
        (program,newExterns,lines) <- parseRoutineBody lines programName []
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
            Left "SYNTAX ERROR"
      | otherwise = do
            let tokens = (tokenize . drop 10 . head) lines
            args <- parseCallArgs (drop 1 tokens)
            let name = head tokens
            ident name
            if Map.member name subs
              then Left ("DUPLICATE SUBROUTINE NAME " ++ name)
              else return ()
            (routine,newExterns,lines) <- parseRoutineBody (tail lines) name args
            if ["END" ++ name] == take 1 lines
              then parseSubs (Map.insert name routine subs) (Set.union externs newExterns) (tail lines)
              else Left "SYNTAX ERROR"

parseRoutineBody :: [String] -> String -> [String] -> Either String (Routine,Set.Set (String,String),[String])
parseRoutineBody lines name args = runST $ do
    varMap <- newSTRef Map.empty
    doIndices <- newSTRef 0
    doEdgesIndices <- newSTRef 0
    doStack <- newSTRef []
    callArgsMaxCountAccum <- newSTRef 0
    externs <- newSTRef Set.empty
    addedArgs <- mapM (addArg varMap) args
    stmtsLines <- parseRoutineStmts lines varMap doIndices doEdgesIndices doStack callArgsMaxCountAccum externs
    finalVarMap <- readSTRef varMap
    finalDoEdgesCount <- readSTRef doEdgesIndices
    finalCallArgsMaxCount <- readSTRef callArgsMaxCountAccum
    finalExterns <- readSTRef externs
    return $ do
        argVars <- sequence addedArgs
        (stmts,lines) <- stmtsLines
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
    addArg varMap arg = do
        vMap <- readSTRef varMap
        if Map.member arg vMap
          then return (Left ("DUPLICATE ARGUMENT " ++ arg))
          else do
            let var = Var arg (fromIntegral $ Map.size vMap)
            writeSTRef varMap (Map.insert arg var vMap)
            return $ do
                ident arg
                return var
    parseRoutineStmts lines varMap doIndices doEdgesIndices doStack callArgsMaxCountAccum externs = 
        parseStmts lines
      where
        getVar varName = do
            vMap <- readSTRef varMap
            case Map.lookup varName vMap of
                Just var -> (return . return) var
                Nothing -> do
                    let var = Var varName (fromIntegral $ Map.size vMap)
                    writeSTRef varMap (Map.insert varName var vMap)
                    return $ do
                        ident varName
                        return var

        getVal valName
          | valName == "0" =
                (return . return) NewVal
          | otherwise = do
                var <- getVar valName
                return (fmap Val var)

        parseStmts :: [String] -> ST a (Either String ([Statement],[String]))
        parseStmts lines = undefined

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

ident :: String -> Either String ()
ident str
  | isIdent str =
        return ()
  | otherwise =
        Left "SYNTAX ERROR"

isIdent :: String -> Bool
isIdent str = str /= "0" && str /= "" && all isAlphaNum str

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
