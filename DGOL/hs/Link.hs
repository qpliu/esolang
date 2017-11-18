module Link(link)
where

import qualified Control.Monad as Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Ast
import qualified Scope

type Subroutine = Scope.Scope -> [Scope.Var] -> IO Scope.Scope

link :: [Ast.Module] -> [(String,Map.Map String Subroutine)] -> Either String (IO ())
link modules stdlibs = do
    (Ast.Program _ _ _ _ prog) <- checkDuplicates modules
    let exports = foldl collectExports Map.empty modules
    let exports = foldl collectStdLibExports exports stdlibs
    mapM_ (checkModuleCalls exports) modules
    let libs = linkLibModules (Map.fromList stdlibs) modules
    let routine = linkRoutine libs "" prog []
    return (routine Scope.new [] >> return ())

checkDuplicates :: [Ast.Module] -> Either String Ast.Module
checkDuplicates modules = do
    (progModule,_) <- Monad.foldM checkModule (Nothing,Set.empty) modules
    maybe (Left "NO PROGRAM MODULE DEFINED") return progModule
  where
    checkModule (Nothing,libraryNames) mod@(Ast.Program _ _ _ _ _) =
        return (Just mod,libraryNames)
    checkModule (_,_) (Ast.Program loc _ _ _ _) =
        locError loc "MULTIPLE PROGRAM MODULES DEFINED"
    checkModule (progModule,libraryNames) (Ast.Library loc name _ _ _)
      | Set.member name libraryNames =
            locError loc "DUPLICATE LIBRARY DEFINITION"
      | otherwise = return (progModule,Set.insert name libraryNames)

collectExports :: Map.Map String (Set.Set String) -> Ast.Module -> Map.Map String (Set.Set String)
collectExports exports (Ast.Program _ _ _ _ _) = exports
collectExports exports (Ast.Library _ name _ _ exportList) =
    Map.insert name (Set.fromList (map fromExport exportList)) exports
  where
    fromExport (Ast.Export _ subroutineName) = subroutineName

collectStdLibExports :: Map.Map String (Set.Set String) -> (String,Map.Map String a) -> Map.Map String (Set.Set String)
collectStdLibExports exports (name,subroutines)
  | Map.member name exports = exports
  | otherwise = Map.insert name (Set.fromList (Map.keys subroutines)) exports

callDefined :: Map.Map String (Set.Set String) -> String -> String -> Bool
callDefined exports libraryName subroutineName
  | libraryName == "" = True
  | otherwise =
        maybe False (Set.member subroutineName) (Map.lookup libraryName exports)

locError :: Ast.Location -> String -> Either String a
locError (Ast.Location filename lineNumber line) msg =
    Left (filename ++ ":" ++ show lineNumber ++ " " ++ msg ++ ": " ++ line)

checkModuleCalls :: Map.Map String (Set.Set String) -> Ast.Module -> Either String ()
checkModuleCalls exports mod =
    case mod of
        (Ast.Program _ _ _ subroutines program) -> do
            mapM_ checkSubroutineCalls subroutines
            mapM_ checkStatementCalls program
        (Ast.Library _ _ _ subroutines _) ->
            mapM_ checkSubroutineCalls subroutines
  where
    checkSubroutineCalls (Ast.Subroutine _ _ _ statements) =
        mapM_ checkStatementCalls statements
    checkStatementCalls (Ast.If ifBranches elseBranch) = do
        mapM_ checkIfBranchCalls ifBranches
        mapM_ checkStatementCalls elseBranch
    checkStatementCalls (Ast.DoLoop _ _ statements) =
        mapM_ checkStatementCalls statements
    checkStatementCalls (Ast.DoEdges _ _ _ statements) =
        mapM_ checkStatementCalls statements
    checkStatementCalls (Ast.Call loc moduleName subroutineName _)
      | moduleName == "" = return ()
      | maybe False (Set.member subroutineName) (Map.lookup moduleName exports) =
            locError loc "UNDEFINED SUBROUTINE"
      | otherwise = return ()
    checkIfBranchCalls (Ast.IfEq _ _ _ statements) =
        mapM_ checkStatementCalls statements
    checkIfBranchCalls (Ast.IfEdge _ _ _ statements) =
        mapM_ checkStatementCalls statements

linkLibModules :: Map.Map String (Map.Map String Subroutine) -> [Ast.Module] -> Map.Map String (Map.Map String Subroutine)
linkLibModules stdlib modules = libs
  where
    libs = foldl linkModule stdlib modules
    linkModule partialLib (Ast.Program _ _ _ subs _) =
        Map.insert "" (foldl (addSubroutine "") Map.empty subs) partialLib
    linkModule partialLib (Ast.Library _ name _ subs _) =
        Map.insert name (foldl (addSubroutine name) Map.empty subs) partialLib
    addSubroutine libName lib (Ast.Subroutine _ subName args statements) =
        Map.insert subName (linkRoutine libs libName statements args) lib

data Exit = Exit Ast.Name | Return | Fallthrough

linkRoutine :: Map.Map String (Map.Map String Subroutine) -> Ast.Name -> [Ast.Statement] -> [Ast.Var] -> Subroutine
linkRoutine libs libName statements params scope args = do
    let scope = Scope.push scope
    let scope = foldl bindParam scope (zip params (map Just args ++ repeat Nothing))
    (_,scope) <- executeStatements scope statements
    (return . Scope.gc . Scope.pop) scope
  where
    bindParam scope (param,Just arg) = Scope.set scope param arg
    bindParam scope (param,Nothing) = snd (Scope.get scope param)
    executeStatements scope [] = do
        return (Fallthrough,scope)
    executeStatements scope (statement:statements) = do
        (exit,scope) <- executeStatement scope statement
        case exit of
            Fallthrough -> executeStatements scope statements
            _ -> return (exit,scope)
    executeStatement scope (Ast.LetEq _ v1 v2) = do
        let (var2,scope2) = Scope.get scope v2
        return (Fallthrough,Scope.set scope2 v1 var2)
    executeStatement scope (Ast.LetAddEdge _ v1 v2) = do
        let (var1,scope1) = Scope.get scope v1
        let (var2,scope2) = Scope.get scope1 v2
        return (Fallthrough,Scope.addEdge scope2 var1 var2)
    executeStatement scope (Ast.LetRemoveEdge _ v1 v2) = do
        let (var1,scope1) = Scope.get scope v1
        let (var2,scope2) = Scope.get scope1 v2
        return (Fallthrough,Scope.removeEdge scope2 var1 var2)
    executeStatement scope (Ast.If ifBranches elseBranch) = do
        executeIf scope ifBranches elseBranch
    executeStatement scope (Ast.Call _ modName subName callArgs) = do
        let (reverseArgs,scope) = foldl evalCallArg ([],scope) callArgs
        let modName = if modName == "" then libName else modName
        scope <- ((libs Map.! modName) Map.! subName) scope (reverse reverseArgs)
        return (Fallthrough,scope)
    executeStatement scope (Ast.Return _) = do
        return (Return,scope)
    executeStatement scope doLoop@(Ast.DoLoop _ label statements) = do
        (exit,scope) <- executeStatements scope statements
        let scope = Scope.gc scope
        case exit of
            Fallthrough -> executeStatement scope doLoop
            Exit exitLabel ->
                if label == exitLabel
                    then return (Fallthrough,scope)
                    else return (exit,scope)
            _ -> return (exit,scope)
    executeStatement scope (Ast.DoEdges _ v1 v2 statements) = do
        let (var2,scope) = Scope.get scope v2
        let (edges,scope) = Scope.edges scope var2
        let scope = Scope.pushDoEdges scope edges
        (exit,scope) <- executeDoEdges scope v1 edges statements
        return (exit,Scope.gc (Scope.popDoEdges scope))
    executeStatement scope (Ast.Exit _ label) = do
        return (Exit label,scope)

    evalCallArg (reverseArgs,scope) callArg =
        let (arg,scope) = Scope.get scope callArg
        in  (arg:reverseArgs,scope)

    executeIf scope [] elseBranch = executeStatements scope elseBranch
    executeIf scope (Ast.IfEq _ v1 v2 statements:ifBranches) elseBranch = do
        let (var1,scope) = Scope.get scope v1
        let (var2,scope) = Scope.get scope v2
        if Scope.eq scope var1 var2
            then executeStatements scope statements
            else executeIf scope ifBranches elseBranch
    executeIf scope (Ast.IfEdge _ v1 v2 statements:ifBranches) elseBranch = do
        let (var1,scope) = Scope.get scope v1
        let (var2,scope) = Scope.get scope v2
        if Scope.hasEdge scope var1 var2
            then executeStatements scope statements
            else executeIf scope ifBranches elseBranch



    executeDoEdges scope v1 [] statements = return (Fallthrough,scope)
    executeDoEdges scope v1 (edge:edges) statements = do
        let scope = Scope.set scope v1 edge
        (exit,scope) <- executeStatements scope statements
        case exit of
            Fallthrough -> executeDoEdges (Scope.gc scope) v1 edges statements
            _ -> return (exit,scope)
