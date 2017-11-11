module Parse(parse)
where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Ast

parse :: String -> String -> Either String Ast.Module
parse filename text = do
    mod <- recursiveDescent filename
        ((filter notEmpty . zip [1..] . map stripSpaces . lines) text)
    check mod
    return mod
  where
    stripSpaces line =
        (line,(filter (not . Char.isSpace) . takeWhile (/= '*')) line)
    notEmpty (_,(_,codeLine)) = codeLine /= ""

type Line = (Integer,(String,String))

recursiveDescent :: String -> [Line] -> Either String Ast.Module
recursiveDescent filename lines = do
    (uses,lines) <- parseUses filename lines
    (subroutines,lines) <- parseSubroutines filename lines
    (loc,name,libOrProg,lines) <- parseLibOrProg filename lines
    Monad.unless (null lines) (returnError filename "SYNTAX ERROR" (head lines))
    return (either (Ast.Library loc name uses subroutines) (Ast.Program loc name uses subroutines) libOrProg)

parseUses :: String -> [Line] -> Either String ([Ast.Use],[Line])
parseUses _ [] = Right ([],[])
parseUses filename lines@(line@(_,(_,codeLine)):rest) =
    tokenize "USE" codeLine parseUse (Right ([],lines))
  where
    parseUse tokens
      | length tokens == 1 && isIdentifier (head tokens) = do
          (uses,lines) <- parseUses filename rest
          return (Ast.Use (location filename line) (head tokens):uses,lines)
      | otherwise = returnError filename "SYNTAX ERROR" line

parseSubroutines :: String -> [Line] -> Either String ([Ast.Subroutine],[Line])
parseSubroutines filename [] = Left (filename ++ " UNEXPECTED EOF")
parseSubroutines filename lines@(line@(_,(_,codeLine)):rest) =
    tokenize "SUBROUTINE" codeLine parseSubroutine (Right ([],lines))
  where
    loc = location filename line
    parseSubroutine tokens
      | length tokens >= 3 && isIdentifier (head tokens) = do
          args <- parseArgs filename line (tail tokens)
          Monad.unless (all isIdentifier args) (returnError filename "SYNTAX ERROR" line)
          Monad.unless (args == List.nub args) (returnError filename "SYNTAX ERROR" line)
          (statements,lines) <- parseStatements filename rest
          lines <- parseEnd filename ("END" ++ head tokens) lines
          (subroutines,lines) <- parseSubroutines filename lines
          return (Ast.Subroutine loc (head tokens) args statements:subroutines,lines)
      | otherwise = returnError filename "SYNTAX ERROR" line

parseLibOrProg :: String -> [Line] -> Either String (Ast.Location,Ast.Name,Either [Ast.Export] [Ast.Statement],[Line])
parseLibOrProg filename [] = Left (filename ++ " UNEXPECTED EOF")
parseLibOrProg filename lines@(line@(_,(_,codeLine)):rest) =
    tokenize "PROGRAM" codeLine parseProgram
        $ tokenize "LIBRARY" codeLine parseLibrary
        $ returnError filename "SYNTAX ERROR" line
  where
    loc = location filename line
    parseProgram [] = returnError filename "SYNTAX ERROR" line
    parseProgram (name:tokens)
      | not (null tokens) || not (isIdentifier name) =
          returnError filename "SYNTAX ERROR" line
      | otherwise = do
          (statements,lines) <- parseStatements filename rest
          lines <- parseEnd filename ("END" ++ name) lines
          return (loc,name,Right statements,lines)
    parseLibrary [] = returnError filename "SYNTAX ERROR" line
    parseLibrary (name:tokens)
      | not (null tokens) || not (isIdentifier name) =
          returnError filename "SYNTAX ERROR" line
      | otherwise = do
          (exports,lines) <- parseExports filename rest
          lines <- parseEnd filename ("END" ++ name) lines
          return (loc,name,Left exports,lines)

returnError :: String -> String -> Line -> Either String a
returnError filename msg (lineNumber,(line,_)) =
    Left (filename ++ ":" ++ show lineNumber ++ " " ++ msg ++ ": " ++ line)

isIdentifier :: String -> Bool
isIdentifier str = isIdentifierOr0 str && str /= "0"

isIdentifierOr0 :: String -> Bool
isIdentifierOr0 str = not (null str) && all Char.isAlphaNum str

tokenize :: String -> String -> ([String] -> a) -> a -> a
tokenize keyword line success fail
  | length keyword > length line = fail
  | keyword /= take (length keyword) line = fail
  | otherwise = success (splitIntoTokens (drop (length keyword) line))
  where
    splitIntoTokens [] = []
    splitIntoTokens line@(c:cs)
      | Char.isAlphaNum c = let (token,rest) = span Char.isAlphaNum line
                            in  token:splitIntoTokens rest
      | otherwise = [c]:splitIntoTokens cs

parseArgs :: String -> Line -> [String] -> Either String [String]
parseArgs filename line tokens
  | take 1 tokens /= ["("] = retError
  | otherwise = parseArg (tail tokens)
  where
    retError = returnError filename "SYNTAX ERROR" line
    parseArg [")"] = return []
    parseArg [token,")"] = return [token]
    parseArg (token:",":tokens) | isIdentifierOr0 token = do
        args <- parseArg tokens
        return (token:args)
    parseArg _ = retError

parseStatements :: String -> [Line] -> Either String ([Ast.Statement],[Line])
parseStatements filename [] = Left (filename ++ " UNEXPECTED EOF")
parseStatements filename lines@(line@(_,(_,codeLine)):rest) =
    tokenize "LET" codeLine (parseNext . parseLet)
        $ tokenize "IF" codeLine (parseNext . parseIf)
        $ tokenize "CALL" codeLine (parseNext . parseCall)
        $ tokenize "RETURN" codeLine (parseNext . parseReturn)
        $ tokenize "DO" codeLine (parseNext . parseDo)
        $ tokenize "EXIT" codeLine (parseNext . parseExit)
        $ return ([],lines)
  where
    parseNext (Left msg) = Left msg
    parseNext (Right (statement,lines)) = do
        (statements,lines) <- parseStatements filename lines
        return (statement:statements,lines)
    parseLet [id1,"=",id2] | isIdentifier id1 && isIdentifierOr0 id2 =
        return (Ast.LetEq (location filename line) id1 id2,rest)
    parseLet [id1,"<",id2] | isIdentifier id1 && isIdentifier id2 =
        return (Ast.LetRemoveEdge (location filename line) id1 id2,rest)
    parseLet [id1,">",id2] | isIdentifier id1 && isIdentifierOr0 id2 =
        return (Ast.LetAddEdge (location filename line) id1 id2,rest)
    parseLet _ = returnError filename "SYNTAX ERROR" line
    parseIf tokens = do
        (ifBranches,lines) <- parseIfBranches filename lines
        (statements,lines) <- parseElseBranch filename lines
        lines <- parseEnd filename "ENDIF" lines
        return (Ast.If ifBranches statements,lines)
    parseCall (lib:".":name:args@("(":_)) | isIdentifier name = do
        args <- parseArgs filename line args
        return (Ast.Call (location filename line) lib name args,rest)
    parseCall (name:args@("(":_)) | isIdentifier name = do
        args <- parseArgs filename line args
        return (Ast.Call (location filename line) "" name args,rest)
    parseCall _ = returnError filename "SYNTAX ERROR" line
    parseReturn [] = return (Ast.Return (location filename line),rest)
    parseReturn _ = returnError filename "SYNTAX ERROR" line
    parseDo [id1] = do
        (statements,lines) <- parseStatements filename rest
        lines <- parseEnd filename "ENDDO" lines
        return (Ast.DoLoop (location filename line) id1 statements,lines)
    parseDo [id1,"<",id2] = do
        (statements,lines@(line@(_,(_,codeLine)):rest)) <- parseStatements filename rest
        lines <- parseEnd filename "ENDDO" lines
        return (Ast.DoEdges (location filename line) id1 id2 statements,lines)
    parseDo _ = returnError filename "SYNTAX ERROR" line
    parseExit [id1] | isIdentifier id1 =
        return (Ast.Exit (location filename line) id1,rest)
    parseExit _ = returnError filename "SYNTAX ERROR" line

parseExports :: String -> [Line] -> Either String ([Ast.Export],[Line])
parseExports filename [] = Left (filename ++ " UNEXPECTED EOF")
parseExports filename lines@(line@(_,(_,codeLine)):rest) =
    tokenize "SUBROUTINE" codeLine parseExport (return ([],lines))
  where
    parseExport tokens
      | length tokens /= 1 || not (isIdentifier (head tokens)) = returnError filename "SYNTAX ERROR" line
      | otherwise = do
          (exports,lines) <- parseExports filename rest
          return (Ast.Export (location filename line) (head tokens):exports,lines)

parseEnd :: String -> String -> [Line] -> Either String [Line]
parseEnd filename end [] = Left (filename ++ " UNEXPECTED EOF")
parseEnd filename end (line@(_,(_,codeLine)):rest)
  | codeLine == end = return rest
  | otherwise = returnError filename "SYNTAX ERROR" line

parseIfBranches :: String -> [Line] -> Either String ([Ast.IfBranch],[Line])
parseIfBranches filename [] = Left (filename ++ " UNEXPECTED EOF")
parseIfBranches filename lines@(line@(_,(_,codeLine)):rest) =
    tokenize "IF" codeLine parseIfBranch
        $ tokenize "ELSEIF" codeLine parseIfBranch
        $ return ([],lines)
  where
    parseIfBranch [id1,"=",id2] = do
        (statements,lines) <- parseStatements filename rest
        (ifBranches,lines) <- parseIfBranches filename lines
        return (Ast.IfEq (location filename line) id1 id2 statements:ifBranches,lines)
    parseIfBranch [id1,">",id2] = do
        (statements,lines) <- parseStatements filename rest
        (ifBranches,lines) <- parseIfBranches filename lines
        return (Ast.IfEdge (location filename line) id1 id2 statements:ifBranches,lines)
    parseIfBranch _ = returnError filename "SYNTAX ERROR" line

parseElseBranch :: String -> [Line] -> Either String ([Ast.Statement],[Line])
parseElseBranch filename [] = Left (filename ++ " UNEXPECTED EOF")
parseElseBranch filename lines@((_,(_,codeLine)):rest)
  | codeLine == "ELSE" = parseStatements filename rest
  | otherwise = return ([],lines)

location :: String -> Line -> Ast.Location
location filename (lineNumber,(line,_)) =
    Ast.Location filename lineNumber line

locError :: Ast.Location -> String -> Either String a
locError (Ast.Location filename lineNumber line) msg =
    Left (filename ++ ":" ++ show lineNumber ++ " " ++ msg ++ ": " ++ line)

check :: Ast.Module -> Either String ()
check mod = checkMod mod
  where
    checkMod (Ast.Library _ _ uses subroutines exports) = do
        useSet <- Monad.foldM checkDuplicateUses Set.empty uses
        subSet <- Monad.foldM checkDuplicateSubroutines Set.empty subroutines
        mapM_ (checkCalls useSet subSet . getStatements) subroutines
        mapM_ (checkExits . getStatements) subroutines
        Monad.foldM_ (checkExports subSet) Set.empty exports
    checkMod (Ast.Program _ _ uses subroutines program) = do
        useSet <- Monad.foldM checkDuplicateUses Set.empty uses
        subSet <- Monad.foldM checkDuplicateSubroutines Set.empty subroutines
        mapM_ (checkCalls useSet subSet . getStatements) subroutines
        mapM_ (checkExits . getStatements) subroutines
        checkCalls useSet subSet program
        checkExits program
        checkReturns program
    checkDuplicateUses useSet (Ast.Use loc name)
      | Set.member name useSet = locError loc "DUPLICATE USE"
      | otherwise = return (Set.insert name useSet)
    checkDuplicateSubroutines subSet (Ast.Subroutine loc name _ _)
      | Set.member name subSet = locError loc "DUPLICATE SUBROUTINE"
      | otherwise = return (Set.insert name subSet)
    getStatements (Ast.Subroutine _ _ _ statements) = statements
    checkCalls useSet subSet statements = undefined
    checkExits statements = undefined
    checkExports subSet exportSet (Ast.Export loc name)
      | Set.member name exportSet = locError loc "DUPLICATE SUBROUTINE"
      | not (Set.member name subSet) = locError loc "UNDEFINED SUBROUTINE"
      | otherwise = return (Set.insert name exportSet)
    checkReturns statements = undefined
