-- https://esolangs.org/wiki/Rail

import Data.Array(Array,array,assocs,(!))
import Data.Map(Map,empty,fold,insert,member,size)
import qualified Data.Map

data Program = Program (Map String Location) (Array Location Char)
type Location = (Int,Int)

parse :: String -> Program
parse prog = Program functions grid
  where
    srcLines = lines prog
    height = length srcLines
    width = maximum (map length srcLines)
    grid = array ((1,1),(width,height)) (concat (zipWith makeGridLine [1..] srcLines))
    makeGridLine y srcLine =
        [((x,y),ch) | (x,ch) <- zip [1..width] (srcLine ++ repeat ' ')]
    functions = foldl findFunction empty [1..height]
    findFunction fns y | grid!(1,y) == '$' = insert (findName y) (1,y) fns
                       | otherwise = fns
    findName y = snd (multiCharCommand grid (x+1,y) E '\'')
      where ((x,_),_) = multiCharCommand grid (1,y) E '\''

data Value = Value String | List [Value] | Lambda CallFrame

newtype ScopeId = ScopeId Int deriving (Eq,Ord)

data Dir = N | NE | E | SE | S | SW | W | NW deriving (Bounded,Enum,Eq,Ord,Show)

type Scope = Map String Value

data CallFrame = CallFrame ScopeId Location Dir

move :: Location -> Dir -> Location
move (x,y) N = (x,y-1)
move (x,y) NE = (x+1,y-1)
move (x,y) E = (x+1,y)
move (x,y) SE = (x+1,y+1)
move (x,y) S = (x,y+1)
move (x,y) SW = (x-1,y+1)
move (x,y) W = (x-1,y)
move (x,y) NW = (x-1,y-1)

turn :: Int -> Dir -> Dir
turn n dir | n == 0 = dir
           | dir == maxBound = turn (mod (n-1) 8) minBound
           | otherwise = turn (mod (n-1) 8) (succ dir)

gc :: Map ScopeId Scope -> [CallFrame] -> Map ScopeId Scope
gc scopes callStack = trace empty callStack
  where
    trace scopesCopy [] = scopesCopy
    trace scopesCopy (CallFrame scopeId _ _:callStack)
      | member scopeId scopesCopy = trace scopesCopy callStack
      | otherwise = trace (insert scopeId scope scopesCopy) (fold pointers callStack scope)
      where
        scope = scopes Data.Map.! scopeId
    pointers (Lambda callFrame) callFrames = callFrame:callFrames
    pointers _ callFrames = callFrames

makeScope :: Map ScopeId Scope -> (ScopeId,Map ScopeId Scope)
makeScope scopes = (scopeId,insert scopeId empty scopes)
  where
    scopeId = findId (size scopes)
    findId n
      | not (member (ScopeId n) scopes) = ScopeId n
      | n == maxBound = findId minBound
      | otherwise = findId (succ n)

multiCharCommand :: Array (Int,Int) Char -> Location -> Dir -> Char -> (Location,String)
multiCharCommand grid location dir end = accumulate location ""
  where
    accumulate loc str | grid!loc == end = (loc,reverse str)
                       | otherwise = accumulate (move loc dir) ((grid!loc):str)
    
nextLocation :: Program -> Location -> Dir -> (Location,Dir)
nextLocation (Program _ grid) loc dir
  | (grid!move loc dir) /= ' ' = (move loc dir,align (move loc dir) dir)
  | (grid!move loc (turn 1 dir)) /= ' ' && (grid!move loc (turn 7 dir)) == ' ' =
      (move loc (turn 1 dir),align (move loc (turn 1 dir)) (turn 1 dir))
  | (grid!move loc (turn 7 dir)) /= ' ' && (grid!move loc (turn 1 dir)) == ' ' =
      (move loc (turn 7 dir),align (move loc (turn 7 dir)) (turn 7 dir))
  | otherwise = crash loc dir
  where
    align loc dir
      | aligned ch dir = dir
      | aligned ch (turn 1 dir) && not (aligned ch (turn 7 dir)) = turn 1 dir
      | aligned ch (turn 7 dir) && not (aligned ch (turn 1 dir)) = turn 7 dir
      | otherwise = crash loc dir
      where ch = grid!loc

crash :: Location -> Dir -> a
crash loc dir = error ("Crash:" ++ show loc ++ "->" ++ show dir)

aligned :: Char -> Dir -> Bool
aligned '-' dir = dir == E || dir == W
aligned '|' dir = dir == N || dir == S
aligned '/' dir = dir == NE || dir == SW
aligned '\\' dir = dir == SE || dir == NW
aligned '+' dir = dir == N || dir == E || dir == S || dir == W
aligned 'x' dir = dir == NE || dir == NW || dir == SE || dir == SW
aligned _ _ = True

interp :: Program -> Map ScopeId Scope -> [CallFrame] -> [Value] -> ScopeId -> (Location,Dir) -> String -> String
interp program@(Program functions grid) scopes callStack valueStack scopeId (location,dir) input = undefined

call :: Program -> Map ScopeId Scope -> [CallFrame] -> [Value] -> String -> String -> String
call program@(Program functions _) scopes callStack valueStack function input =
    interp program newScopes callStack valueStack newScopeId (functions Data.Map.! function,SE) input
  where (newScopeId,newScopes) = makeScope scopes

run :: String -> String -> String
run prog input = call (parse prog) empty [] [] "main" input

rail :: String -> IO ()
rail prog = interact (run prog)
