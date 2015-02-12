import Data.Char(chr,isDigit,isSpace)
import Data.Map(Map)
import qualified Data.Map as M

parse :: String -> Map Integer [Stmt]
parse = foldl parseLine (M.fromList [(0,[line0]),(1,[line1])]) . lines

parseLine :: Map Integer [Stmt] -> String -> Map Integer [Stmt]
parseLine prog line
  | take 3 lineContents == "REM" = prog
  | null lineNum = error "SYNTAX ERROR"
  | take 4 lineContents /= "GOTO" = addStmt (error "SYNTAX ERROR")
  | null targetLineNum = addStmt (error "SYNTAX ERROR")
  | not (null trailingGarbage) = addStmt (error "SYNTAX ERROR")
  | otherwise = addStmt (goto (read targetLineNum))
  where
    (lineNum,lineContents) =
        break (not . isDigit) (filter (not . isSpace) line)
    (targetLineNum,trailingGarbage) =
        break (not . isDigit) (drop 4 lineContents)
    addStmt stmt = M.alter (Just . (stmt:) . maybe [] id) (read lineNum) prog


type Stmt = State -> State

data State = State{
    stProg :: Map Integer [Stmt],
    st0 :: Integer,
    st1 :: Int,
    stThreads :: [Stmt]
    }

goto :: Integer -> Stmt
goto target st@State{stProg = prog, stThreads = threads} =
    maybe st update (M.lookup target prog)
  where update stmts = st{stThreads = stmts ++ threads}

line0 :: Stmt
line0 st@State{st0 = n} = st{st0 = n + 1}

line1 :: Stmt
line1 st@State{st1 = n} = st{st1 = n + 1}


run :: State -> [Bool]
run st@State{stThreads = threads}
  | null threads = []
  | otherwise = output n1 ++ run st''{st0 = 0, st1 = 0}
  where
    st'@State{st0 = n0} = foldl (flip ($)) st{stThreads = []} threads
    st''@State{st1 = n1} | n0 == 0 = st' | otherwise = goto n0 st'
    output 0 = []
    output 1 = [False]
    output n = bits (n-1)
    bits 0 = []
    bits n = (n `mod` 2 == 1) : bits (n `div` 2)

toStr :: [Bool] -> String
toStr b | null b = [] | otherwise = toChr (take 8 b) : toStr (drop 8 b)
  where toChr b = chr (sum (zipWith select (reverse b) (iterate (2*) 1)))
        select b n | b = n | otherwise = 0


goto10 :: String -> String
goto10 = (toStr . run . goto 10 . init)
  where
    init prog = State{stProg = parse prog, stThreads = [], st0 = 0, st1 = 0}

main :: IO ()
main = interact goto10
