-- https://esolangs.org/wiki/Kipple

-- Build: ghc --make kipple
-- Usage: kipple SRC-FILE

import Data.Char(chr,ord,toLower)
import Data.Map(Map,adjust,fromList,(!))
import System.Environment(getArgs)

type Id = Char
type State = Map Id [Int]

data Code = Push Id Operand | Add Id Operand | Subtract Id Operand | Clear Id | Loop Id [Code] deriving Show
data Operand = Number Int | Stack Id deriving Show

data Token = StackToken Id | NumberToken Int | OpToken Char | OpenLoop | CloseLoop | JunkToken Char deriving Show

tokenize :: String -> [Token]
tokenize "" = []
tokenize ('#':src) = JunkToken '#' : tokenize (dropWhile (/= '\n') src)
tokenize ('(':src) = OpenLoop : tokenize src
tokenize (')':src) = CloseLoop : tokenize src
tokenize (c:src)
  | toLower c `elem` '@':['a'..'z'] = StackToken (toLower c) : tokenize src
  | c `elem` ['0'..'9'] = numberToken (ord c - ord '0') src
  | c `elem` ['<','>','+','-','?'] = OpToken c : tokenize src
  | otherwise = JunkToken c : tokenize src
  where
    numberToken n "" = [NumberToken n]
    numberToken n (c:src)
      | c `elem` ['0'..'9'] = numberToken (10*n + ord c - ord '0') src
      | otherwise = NumberToken n : tokenize (c:src)

parse :: [Token] -> [Code]
parse tokens = prog
  where
    ([],prog) = parseCode tokens []
    parseCode [] code = ([],reverse code)
    parseCode (OpenLoop:toks@(StackToken stackId:_)) code =
        let ((CloseLoop:toks2),body) = parseCode toks []
        in  parseCode toks2 (Loop stackId body:code)
    parseCode (OpenLoop:_) code = error "Syntax error"
    parseCode toks@(CloseLoop:_) code = (toks,reverse code)
    parseCode (StackToken stackId:OpToken '<':toks@(StackToken stackId2:_)) code =
        parseCode toks (Push stackId (Stack stackId2):code)
    parseCode (StackToken stackId:OpToken '<':toks@(NumberToken n:_)) code =
        parseCode toks (Push stackId (Number n):code)
    parseCode (_:OpToken '<':_) code = error "Syntax error"
    parseCode (StackToken stackId:OpToken '>':toks@(StackToken stackId2:_)) code =
        parseCode toks (Push stackId2 (Stack stackId):code)
    parseCode (NumberToken n:OpToken '>':toks@(StackToken stackId:_)) code =
        parseCode toks (Push stackId (Number n):code)
    parseCode (_:OpToken '>':_) code = error "Syntax error"
    parseCode (StackToken stackId:OpToken '+':toks@(StackToken stackId2:_)) code =
        parseCode toks (Add stackId (Stack stackId2):code)
    parseCode (StackToken stackId:OpToken '+':toks@(NumberToken n:_)) code =
        parseCode toks (Add stackId (Number n):code)
    parseCode (_:OpToken '+':_) code = error "Syntax error"
    parseCode (StackToken stackId:OpToken '-':toks@(StackToken stackId2:_)) code =
        parseCode toks (Subtract stackId (Stack stackId2):code)
    parseCode (StackToken stackId:OpToken '-':toks@(NumberToken n:_)) code =
        parseCode toks (Subtract stackId (Number n):code)
    parseCode (_:OpToken '-':_) code = error "Syntax error"
    parseCode (StackToken stackId:OpToken '?':toks) code =
        parseCode toks (Clear stackId:code)
    parseCode (_:OpToken '?':_) code = error "Syntax error"
    parseCode (_:toks) code = parseCode toks code

empty :: State -> Id -> Bool
empty state stackId = null (state!stackId)

clear :: State -> Id -> State
clear state stackId = adjust (const []) stackId state

top :: State -> Id -> Int
top state stackId = head (state!stackId ++ [0])

pop :: State -> Id -> State
pop state stackId = adjust (drop 1) stackId state

push :: State -> Int -> Id -> State
push state n stackId
  | stackId == '@' = adjust (map ord (reverse (show n)) ++) stackId state
  | otherwise = adjust (n:) stackId state

operand :: State -> Operand -> (Int,State)
operand state (Number n) = (n,state)
operand state (Stack stackId) = (top state stackId,pop state stackId)

interp :: State -> [Code] -> State
interp state [] = state
interp state (Push stackId o:code) =
    let (n,state2) = operand state o
    in  interp (push state2 n stackId) code
interp state (Add stackId o:code) =
    let (n,state2) = operand state o
    in  interp (push state2 (top state stackId + n) stackId) code
interp state (Subtract stackId o:code) =
    let (n,state2) = operand state o
    in  interp (push state2 (top state stackId - n) stackId) code
interp state (Clear stackId:code)
  | top state stackId == 0 = interp (clear state stackId) code
  | otherwise = interp state code
interp state loop@(Loop stackId body:code)
  | empty state stackId = interp state code
  | otherwise = interp (interp state body) loop

run :: String -> String -> String
run prog inp = map chr (state!'o')
  where
    state = interp (adjust (const (map ord (reverse inp))) 'i' init) (parse (tokenize prog))
    init = fromList (('@',[]):zip ['a'..'z'] (repeat []))

kipple :: String -> IO ()
kipple prog = interact (run prog)

main :: IO ()
main = getArgs >>= readFile . head >>= kipple
