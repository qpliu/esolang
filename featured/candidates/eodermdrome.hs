-- https://esolangs.org/wiki/Eodermdrome

-- Build: ghc --make eodermdrome
-- Usage: eodermdrome SRC-FILE

import Data.Char(ord)
import Data.List(permutations)
import Data.Set(Set,difference,elems,empty,findMin,fromList,isSubsetOf,member,size,union)
import qualified Data.Set as S
import System.Environment(getArgs)

data Command = Command {
    input :: Set Char,
    match :: Set (Int,Int),
    minMatch :: Int,
    output :: String,
    replacement :: Set (Int,Int),
    removals :: Set Int
    }
  deriving Show

skipSpace :: String -> String
skipSpace str
  | null str = str
  | head str `elem` " \t\r\n.?'\";:![]{}-" = skipSpace (tail str)
  | head str == ',' = skipSpace (drop 1 (dropWhile (/= ',') (tail str)))
  | otherwise = str

parseIO :: String -> (String,String)
parseIO str
  | take 2 str == "()" = (')':takeWhile (/= ')') (drop 2 str),drop 1 (dropWhile (/= ')') (drop 2 str)))
  | take 1 str == "(" = (takeWhile (/= ')') (drop 1 str),drop 1 (dropWhile (/= ')') (drop 1 str)))
  | otherwise = ("",str)

parseGraph :: String -> ((Set (Int,Int),Set Int),String)
parseGraph str = (((fromList . pairs . map toInt) graph,(fromList . map toInt) graph),rest)
  where
    (graph,rest) = span (`elem` ['a' .. 'z']) str
    toInt c = ord c - 97
    pairs [] = []
    pairs [_] = []
    pairs (a:b:c) = (min a b,max a b) : pairs (b:c)

parse :: String -> [Command]
parse str
  | input == "" && matchElems == empty && output == "" && replacementElems == empty = []
  | replacementElems == empty = error "Unexpected EOF"
  | otherwise = Command{input=fromList input,match=match,minMatch=findMin matchElems,output=output,replacement=replacement,removals=removals} : parse str5
  where
    (input,str2) = parseIO (skipSpace str)
    ((match,matchElems),str3) = parseGraph (skipSpace str2)
    (output,str4) = parseIO (skipSpace str3)
    ((replacement,replacementElems),str5) = parseGraph (skipSpace str4)
    removals = matchElems `difference` replacementElems

nodes :: Set (Int,Int) -> [Int]
nodes g = elems (union (S.map fst g) (S.map snd g))

makeNodeMapper :: [(Int,Int)] -> Int -> Int
makeNodeMapper assocs node = maybe (-node) id (lookup node assocs)

makePairMapper :: (Int -> Int) -> Set (Int,Int) -> Set (Int,Int)
makePairMapper mapper = S.map pairMapper
  where
    pairMapper (a,b) = (min (mapper a) (mapper b),max (mapper a) (mapper b))

compact :: Set (Int,Int) -> Set (Int,Int)
compact g = makePairMapper (makeNodeMapper (zip (nodes g) [0..])) g

remove :: Set Int -> Set (Int,Int) -> Set (Int,Int)
remove removals g = S.filter (not . test) g
  where
    test (a,b) = a `member` removals || b `member` removals

findMapper :: Set (Int,Int) -> Command -> Maybe (Int -> Int)
findMapper g Command{match=match,minMatch=minMatch}
  | size g < size match = Nothing
  | match == empty = Just (makeNodeMapper [(minMatch,0)])
  | otherwise = search (permutations (nodes g))
  where
    search [] = Nothing
    search (perm:rest)
      | makePairMapper mapper match `isSubsetOf` g = Just mapper
      | otherwise = search rest
      where
        mapper = makeNodeMapper (zip (nodes match) perm)

interp :: [Command] -> Set (Int,Int) -> [Command] -> String -> String
interp prog state [] inp = ""
interp prog state (cmd@Command{input=input,match=match,output=output,replacement=replacement,removals=removals}:cmds) inp
  | input == empty = maybe next (replace inp) (findMapper state cmd)
  | null inp = next
  | head inp `member` input = maybe next (replace (tail inp)) (findMapper state cmd)
  | otherwise = next
  where
    next = interp prog state cmds inp
    replace inp2 mapper = output ++ interp prog (compact (nextState mapper)) prog inp2
    nextState mapper = (union (makePairMapper mapper replacement) . remove (S.map mapper removals)) (state `difference` makePairMapper mapper match)

run :: String -> String -> String
run prog inp = interp cmds ((fst . fst . parseGraph) "thequickbrownfoxjumpsoverthelazydog") cmds inp
  where cmds = parse prog

eodermdrome :: String -> IO ()
eodermdrome prog = interact (run prog)

main :: IO ()
main = getArgs >>= readFile . head >>= eodermdrome
