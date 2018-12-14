-- https://esolangs.org/wiki/Befunge

import Data.Array(Array,array,(!),(//))
import Data.Char(chr,ord)
import System.Environment(getArgs)
import System.Random(RandomGen(next),StdGen,newStdGen)

data State = State {
    dir :: (Int,Int),
    pc :: (Int,Int),
    codespace :: Array (Int,Int) Int,
    stack :: [Int],
    stringmode :: Bool,
    rand :: StdGen,
    halted :: Bool
    }

parse :: String -> StdGen -> State
parse prog rand = State {
    dir = (1,0),
    pc = (0,0),
    codespace = array ((0,0),(79,24)) (concat (enumerate grid)),
    stack = [],
    stringmode = False,
    rand = rand,
    halted = False
    }
  where
    grid = map (take 80 . (++ repeat 32))
               (take 25 (map (map ord) (lines prog) ++ repeat []))
    enumerateLine y line = zip (map (flip (,) y) [0..]) line
    enumerate = zipWith enumerateLine [0..]

nextPc :: State -> State
nextPc state@State{dir = (dx,dy), pc = (x,y)}
  | x + dx < 0 = state{pc = (79,y+dy)}
  | x + dx >= 80 = state{pc = (0,y+dy)}
  | y + dy < 0 = state{pc = (x+dx,24)}
  | y + dy >= 25 = state{pc = (x+dx,0)}
  | otherwise = state{pc = (x+dx,y+dy)}

step :: State -> IO State
step state@State{pc = pc, codespace = codespace, stack = stack, stringmode = stringmode}
  | stringmode && codespace ! pc == 34 = return (nextPc state { stringmode = False })
  | stringmode = return (nextPc state { stack = codespace ! pc : stack })
  | otherwise = fmap nextPc (insn (codespace ! pc) state)

insn :: Int -> State -> IO State
insn cmd state@State{codespace = codespace, stack = stack, rand = rand}
  | cmd == 43 = binop (+)
  | cmd == 45 = binop (-)
  | cmd == 42 = binop (*)
  | cmd == 47 = binop (\ a b -> if b == 0 then 0 else a `div` b)
  | cmd == 37 = binop (\ a b -> if b == 0 then 0 else a `mod` b)
  | cmd == 33 = return state{stack = (if pop == 0 then 1 else 0) : popstack}
  | cmd == 96 = binop (\ a b -> if a > b then 1 else 0)
  | cmd == 62 = return state{dir = (1,0)}
  | cmd == 60 = return state{dir = (-1,0)}
  | cmd == 94 = return state{dir = (0,-1)}
  | cmd == 118 = return state{dir = (0,1)}
  | cmd == 63 = let (n,rand2) = next rand in return state{dir = if n `mod` 4 == 0 then (1,0) else if n `mod` 4 == 1 then (-1,0) else if n `mod` 4 ==2 then (0,1) else (0,-1), rand = rand2}
  | cmd == 95 = return state{stack = popstack, dir = (if pop == 0 then 1 else -1,0)}
  | cmd == 124 = return state{stack = popstack, dir = (0,if pop == 0 then 1 else -1)}
  | cmd == 34 = return state{stringmode = True}
  | cmd == 58 = return state{stack = pop:pop:popstack}
  | cmd == 92 = if null popstack
        then return state
        else return state{stack = head popstack:pop:drop 2 stack}
  | cmd == 36 = return state{stack = popstack}
  | cmd == 46 = do
        putStr (show pop)
        putStr " "
        return state{stack = popstack}
  | cmd == 44 = do
        putChar (chr pop)
        return state{stack = popstack}
  | cmd == 35 = return (nextPc state)
  | cmd == 103 = binop (\ x y -> if x < 0 || y < 0 || x >= 80 || y >= 25 then 0 else codespace ! (x,y))
  | cmd == 112 = case stack of
        y:x:v:rest -> if x < 0 || y < 0 || x >= 80 || y >= 25
            then return state{stack = rest}
            else return state{codespace = codespace // [((x,y),v)], stack = rest}
        [y,x] -> if x < 0 || y < 0 || x >= 80 || y >= 25
            then return state{stack = []}
            else return state{codespace = codespace // [((x,y),0)], stack = []}
        [y] -> if y < 0 || y >= 25
            then return state{stack = []}
            else return state{codespace = codespace // [((0,y),0)], stack = []}
        [] -> return state{codespace = codespace // [((0,0),0)]}
  | cmd == 38 = do
        line <- getLine
        return state{stack = read line:stack}
  | cmd == 126 = do
        ch <- getChar
        return state{stack = ord ch:stack}
  | cmd == 64 = return state{halted = True}
  | cmd >= 48 && cmd <= 57 = return state{stack = cmd - 48:stack}
  | otherwise = return state
  where
    pop | null stack = 0 | otherwise = head stack
    popstack = drop 1 stack
    binop op
      | null popstack = return state{stack = [op 0 pop]}
      | otherwise = return state{stack = op (head popstack) pop : drop 2 stack}

run :: State -> IO ()
run state@State{halted = halted}
  | halted = return ()
  | otherwise = step state >>= run

befunge93 :: String -> IO ()
befunge93 prog = newStdGen >>= run . parse prog
