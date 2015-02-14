import Data.Char(chr,isDigit,isSpace)
import Data.Map(Map)
import qualified Data.Map as M

parse :: String -> Map Integer [Integer]
parse = foldl parseLine (M.fromList [(0,[]),(1,[])]) . lines

parseLine :: Map Integer [Integer] -> String -> Map Integer [Integer]
parseLine prog line
  | take 3 lineContents == "REM" = prog
  | null lineNum = error "SYNTAX ERROR"
  | take 4 lineContents /= "GOTO" = error "SYNTAX ERROR"
  | null targetLineNum = error "SYNTAX ERROR"
  | not (null trailingGarbage) = error "SYNTAX ERROR"
  | otherwise =
      M.alter (Just . (read targetLineNum:) . maybe [] id) (read lineNum) prog
  where
    (lineNum,lineContents) =
        break (not . isDigit) (filter (not . isSpace) line)
    (targetLineNum,trailingGarbage) =
        break (not . isDigit) (drop 4 lineContents)

run :: Map Integer Integer -> Map Integer [Integer] -> [Bool]
run st prog
  | M.fold (+) 0 st == 0 = []
  | otherwise = output (st'' M.! 1) ++ run st'' prog
  where
    st' = M.foldrWithKey goto (M.map (const 0) prog) st
    st'' | st' M.! 0 == 0 = st'
         | otherwise = M.alter (fmap (1+)) (st' M.! 0) st'
    output n | n == 0 = [] | n == 1 = [False] | otherwise = bits (n-1)
    bits n | n == 0 = [] | otherwise = (n `mod` 2 == 1) : bits (n `div` 2)
    goto from n state = foldl (flip $ M.alter (fmap (+n))) state
                              (maybe [] id (M.lookup from prog))

toStr :: [Bool] -> String
toStr b | null b = [] | otherwise = toChr (take 8 b) : toStr (drop 8 b)
  where toChr b = chr (sum (zipWith select (reverse b) (iterate (2*) 1)))
        select b n | b = n | otherwise = 0

goto10 :: String -> String
goto10 = toStr . run (M.fromList [(10,1)]) . parse

main :: IO ()
main = interact goto10
