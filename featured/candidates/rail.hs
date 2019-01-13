-- https://esolangs.org/wiki/Rail

import Data.Array(Array,array,assocs,bounds,inRange)
import qualified Data.Array
import Data.Char(isDigit)
import Data.Map(Map,empty,fold,insert,member,size,(!))

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
    findFunction fns y | grid Data.Array.! (1,y) == '$' = insert (findName y) (1,y) fns
                       | otherwise = fns
    findName y = snd (multiCharCommand grid (x+1,y) E '\'')
      where ((x,_),_) = multiCharCommand grid (1,y) E '\''

data Value = Value String | List [Value] | Lambda CallFrame deriving Eq

newtype ScopeId = ScopeId Int deriving (Eq,Ord)

data Dir = N | NE | E | SE | S | SW | W | NW deriving (Bounded,Enum,Eq,Ord,Show)

type Scope = Map String Value

data CallFrame = CallFrame ScopeId (Location,Dir) deriving Eq

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
    trace scopesCopy (CallFrame scopeId _:callStack)
      | member scopeId scopesCopy = trace scopesCopy callStack
      | otherwise = trace (insert scopeId scope scopesCopy) (fold pointers callStack scope)
      where
        scope = scopes!scopeId
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
multiCharCommand grid loc dir end = accumulate loc ""
  where
    (!) = (Data.Array.!)
    accumulate loc str | grid!loc == end = (loc,reverse str)
                       | otherwise = accumulate (move loc dir) ((grid!loc):str)

stringConst :: Array (Int,Int) Char -> Location -> Dir -> Char -> (Location,String)
stringConst grid loc dir end = accumulate loc ""
  where
    (!) = (Data.Array.!)
    accumulate loc str
      | grid!loc == end = (loc,reverse str)
      | grid!loc == '\\' && grid!loc1 == '\\' && grid!loc2 == '\\' = accumulate loc3 ('\\':str)
      | grid!loc == '\\' && grid!loc1 == '[' && grid!loc2 == '\\' = accumulate loc3 ('[':str)
      | grid!loc == '\\' && grid!loc1 == ']' && grid!loc2 == '\\' = accumulate loc3 (']':str)
      | grid!loc == '\\' && grid!loc1 == 'n' && grid!loc2 == '\\' = accumulate loc3 ('\n':str)
      | grid!loc == '\\' && grid!loc1 == 't' && grid!loc2 == '\\' = accumulate loc3 ('\t':str)
      | otherwise = accumulate loc1 ((grid!loc):str)
      where
        loc1 = move loc dir
        loc2 = move loc1 dir
        loc3 = move loc2 dir

nextLocation :: Program -> Location -> Dir -> (Location,Dir)
nextLocation (Program _ grid) loc dir
  | elt (move loc dir) /= ' ' = (move loc dir,align (move loc dir) dir)
  | elt (move loc (turn 1 dir)) /= ' ' && elt (move loc (turn 7 dir)) == ' ' =
      (move loc (turn 1 dir),align (move loc (turn 1 dir)) (turn 1 dir))
  | elt (move loc (turn 7 dir)) /= ' ' && elt (move loc (turn 1 dir)) == ' ' =
      (move loc (turn 7 dir),align (move loc (turn 7 dir)) (turn 7 dir))
  | otherwise = crash loc dir
  where
    (!) = (Data.Array.!)
    align loc dir
      | aligned ch dir = dir
      | aligned ch (turn 1 dir) && not (aligned ch (turn 7 dir)) = turn 1 dir
      | aligned ch (turn 7 dir) && not (aligned ch (turn 1 dir)) = turn 7 dir
      | otherwise = crash loc dir
      where ch = grid!loc
    elt loc | inRange (bounds grid) loc = grid!loc | otherwise = ' '

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
interp program@(Program functions grid) scopes callStack valueStack scopeId (loc,dir) input = exec (grid Data.Array.! loc)
  where
    exec ch
      | ch `elem` "$-|/\\*+x" = interp program scopes callStack valueStack scopeId (nextLocation program loc dir) input
      | ch == 'v' && dir == SE && is_bool 0 = interp program scopes callStack (tail valueStack) scopeId (nextLocation program loc (if bool 0 then S else NE)) input
      | ch == 'v' && dir == SW && is_bool 0 = interp program scopes callStack (tail valueStack) scopeId (nextLocation program loc (if bool 0 then NW else S)) input
      | ch == 'v' && dir == N && is_bool 0 = interp program scopes callStack (tail valueStack) scopeId (nextLocation program loc (if bool 0 then NE else NW)) input
      | ch == '>' && dir == SE && is_bool 0 = interp program scopes callStack (tail valueStack) scopeId (nextLocation program loc (if bool 0 then SW else E)) input
      | ch == '>' && dir == W && is_bool 0 = interp program scopes callStack (tail valueStack) scopeId (nextLocation program loc (if bool 0 then NW else SW)) input
      | ch == '>' && dir == NE && is_bool 0 = interp program scopes callStack (tail valueStack) scopeId (nextLocation program loc (if bool 0 then E else NW)) input
      | ch == '^' && dir == S && is_bool 0 = interp program scopes callStack (tail valueStack) scopeId (nextLocation program loc (if bool 0 then SW else SE)) input
      | ch == '^' && dir == NW && is_bool 0 = interp program scopes callStack (tail valueStack) scopeId (nextLocation program loc (if bool 0 then N else SW)) input
      | ch == '^' && dir == NE && is_bool 0 = interp program scopes callStack (tail valueStack) scopeId (nextLocation program loc (if bool 0 then SE else N)) input
      | ch == '<' && dir == E && is_bool 0 = interp program scopes callStack (tail valueStack) scopeId (nextLocation program loc (if bool 0 then SE else NE)) input
      | ch == '<' && dir == SW && is_bool 0 = interp program scopes callStack (tail valueStack) scopeId (nextLocation program loc (if bool 0 then W else SE)) input
      | ch == '<' && dir == NW && is_bool 0 = interp program scopes callStack (tail valueStack) scopeId (nextLocation program loc (if bool 0 then NE else W)) input
      | ch == '[' = pushConst (stringConst grid (move loc dir) dir ']')
      | ch == ']' = pushConst (stringConst grid (move loc dir) dir '[')
      | ch == '(' = varRef (multiCharCommand grid (move loc dir) dir ')')
      | ch == ')' = varRef (multiCharCommand grid (move loc dir) dir '(')
      | ch == '{' = funcRef (multiCharCommand grid (move loc dir) dir '}')
      | ch == '}' = funcRef (multiCharCommand grid (move loc dir) dir '{')
      | ch == 'b' && is_string 0 = string 0 ++ crash loc dir
      | ch == 'e' = interp program scopes callStack (Value (if null input then "1" else "0"):valueStack) scopeId (nextLocation program loc dir) input
      | ch == 'i' && not (null input) = interp program scopes callStack (Value (take 1 input):valueStack) scopeId (nextLocation program loc dir) (tail input)
      | ch == 'o' && is_string 0 = string 0 ++ interp program scopes callStack (tail valueStack) scopeId (nextLocation program loc dir) input
      | ch == 'u' = interp program scopes callStack (Value (show (length valueStack)):valueStack) scopeId (nextLocation program loc dir) input
      | ch == '?' && is_string 0 = interp program scopes callStack (Value "string":tail valueStack) scopeId (nextLocation program loc dir) input
      | ch == '?' && is_nil 0 = interp program scopes callStack (Value "nil":tail valueStack) scopeId (nextLocation program loc dir) input
      | ch == '?' && is_list 0 = interp program scopes callStack (Value "list":tail valueStack) scopeId (nextLocation program loc dir) input
      | ch == '?' && is_lambda 0 = interp program scopes callStack (Value "lambda":tail valueStack) scopeId (nextLocation program loc dir) input
      | ch == 'a' && is_numeric 0 && is_numeric 1 = interp program scopes callStack (Value (show (numeric 1 + numeric 0)):drop 2 valueStack) scopeId (nextLocation program loc dir) input
      | ch == 'd' && is_numeric 0 && is_numeric 1 = interp program scopes callStack (Value (show (numeric 1 `div` numeric 0)):drop 2 valueStack) scopeId (nextLocation program loc dir) input
      | ch == 'm' && is_numeric 0 && is_numeric 1 = interp program scopes callStack (Value (show (numeric 1 * numeric 0)):drop 2 valueStack) scopeId (nextLocation program loc dir) input
      | ch == 'r' && is_numeric 0 && is_numeric 1 = interp program scopes callStack (Value (show (numeric 1 `rem` numeric 0)):drop 2 valueStack) scopeId (nextLocation program loc dir) input
      | ch == 's' && is_numeric 0 && is_numeric 1 = interp program scopes callStack (Value (show (numeric 1 - numeric 0)):drop 2 valueStack) scopeId (nextLocation program loc dir) input
      | isDigit ch = interp program scopes callStack (Value [ch]:valueStack) scopeId (nextLocation program loc dir) input
      | ch == 'c' && is_string 1 && is_numeric 0 && numeric 0 >= 0 && numeric 0 <= fromIntegral (length (string 1)) = interp program scopes callStack (Value (drop (fromIntegral (numeric 0)) (string 1)):Value (take (fromIntegral (numeric 0)) (string 1)):drop 2 valueStack) scopeId (nextLocation program loc dir) input
      | ch == 'p' && is_string 0 && is_string 1 = interp program scopes callStack (Value (string 1 ++ string 0):drop 2 valueStack) scopeId (nextLocation program loc dir) input
      | ch == 'z' && is_string 0 = interp program scopes callStack (Value (show (length (string 0))):drop 1 valueStack) scopeId (nextLocation program loc dir) input
      | ch == 'n' = interp program scopes callStack (List []:valueStack) scopeId (nextLocation program loc dir) input
      | ch == ':' && is_list 1 = interp program scopes callStack (List (head valueStack:list 1):drop 2 valueStack) scopeId (nextLocation program loc dir) input
      | ch == '~' && is_list 0 && not (is_nil 0) = interp program scopes callStack (head (list 0):List (tail (list 0)):drop 1 valueStack) scopeId (nextLocation program loc dir) input
      | ch == 'f' = interp program scopes callStack (Value "0":valueStack) scopeId (nextLocation program loc dir) input
      | ch == 'g' && is_numeric 0 && is_numeric 1 = interp program scopes callStack (Value (if numeric 1 > numeric 0 then "1" else "0"):drop 2 valueStack) scopeId (nextLocation program loc dir) input
      | ch == 'q' && length valueStack >= 2 = interp program scopes callStack (Value (if head valueStack == head (tail valueStack) then "1" else "0"):drop 2 valueStack) scopeId (nextLocation program loc dir) input
      | ch == 't' = interp program scopes callStack (Value "1":valueStack) scopeId (nextLocation program loc dir) input
      | ch == '@' = interp program scopes callStack valueStack scopeId (nextLocation program loc (turn 4 dir)) input
      | ch == '#' && null callStack = ""
      | ch == '#' = let CallFrame oldScopeId oldOrientation:oldCallStack = callStack
                    in  interp program (gc scopes oldCallStack) oldCallStack valueStack oldScopeId oldOrientation input
      | ch == '&' = interp program scopes callStack (Lambda (CallFrame scopeId (nextLocation program loc dir)):valueStack) scopeId (nextLocation program loc (turn 4 dir)) input
      | otherwise = crash loc dir
    is_string n = case drop n valueStack of { (Value _:_) -> True; _ -> False }
    string n = let (Value str:_) = drop n valueStack in str
    is_numeric n = is_string n && ((take 1 (string n) == "-" && all isDigit (drop 1 (string n))) || all isDigit (string n))
    numeric n = read (string n) :: Integer
    is_bool n = is_string n && (string n == "0" || string n == "1")
    bool n = string n == "1"
    is_list n = case drop n valueStack of { (List _:_) -> True; _ -> False }
    list n = let (List l:_) = drop n valueStack in l
    is_nil n = is_list n && null (list n)
    is_lambda n = case drop n valueStack of { (Lambda _:_) -> True; _ -> False }
    lambda n = let (Lambda callFrame:_) = drop n valueStack in callFrame
    pushConst (loc,str) = interp program scopes callStack (Value str:valueStack) scopeId (nextLocation program loc dir) input
    varRef (loc,str)
      | length str >= 2 && head str == '!' && last str == '!' && not (null valueStack) = interp program (insert scopeId (insert (reverse (drop 1 (reverse (drop 1 str)))) (head valueStack) (scopes!scopeId)) scopes) callStack (tail valueStack) scopeId (nextLocation program loc dir) input
      | member str (scopes!scopeId) = interp program scopes callStack (((scopes!scopeId)!str):valueStack) scopeId (nextLocation program loc dir) input
      | otherwise = crash loc dir
    funcRef (loc,str)
      | not (null str) = call program scopes (CallFrame scopeId (nextLocation program loc dir):callStack) valueStack str input
      | is_lambda 0 = let (Lambda (CallFrame newScopeId newOrientation):newValueStack) = valueStack in interp program scopes (CallFrame scopeId (nextLocation program loc dir):callStack) newValueStack newScopeId newOrientation input
      | otherwise = crash loc dir

call :: Program -> Map ScopeId Scope -> [CallFrame] -> [Value] -> String -> String -> String
call program@(Program functions _) scopes callStack valueStack function input =
    interp program newScopes callStack valueStack newScopeId (functions!function,SE) input
  where (newScopeId,newScopes) = makeScope scopes

run :: String -> String -> String
run prog input = call (parse prog) empty [] [] "main" input

rail :: String -> IO ()
rail prog = interact (run prog)
