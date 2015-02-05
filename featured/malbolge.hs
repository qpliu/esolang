-- http://esolangs.org/wiki/Malbolge

import Data.Array(Array,listArray,(!),(//))
import Data.Char(chr,isSpace,ord)

trits :: [Int]
trits = take 10 $ iterate (*3) 1

toInt :: [Int] -> Int
toInt word = sum $ zipWith (*) trits word

fromInt :: Int -> [Int]
fromInt i = map ((`mod` 3) . (i `div`)) trits

wordOp :: [Int] -> [Int] -> [Int]
wordOp a b = zipWith op' a b
  where
    op' 0 0 = 1
    op' 0 1 = 1
    op' 0 2 = 2
    op' 1 0 = 0
    op' 1 1 = 0
    op' 1 2 = 2
    op' 2 0 = 0
    op' 2 1 = 2
    op' 2 2 = 1

op :: Int -> Int -> Int
op a b = toInt (fromInt a `wordOp` fromInt b)

rotate :: Int -> Int
rotate a = (a `mod` 3) * 3^9 + a `div` 3

data State = State Int Int Int (Array Int Int)

table :: Array Int Int
table = listArray (0,93) (map ord "5z]&gqtyfr$(we4{WP)H-Zn,[%\\3dL+Q;>U!pJS72FhOA1CB6v^=I_0/8|jsb9m<.TVac`uY*MK'X~xDl}REokN:#?G\"i@")

insn :: Int -> Maybe (State -> String -> String)
insn 4 = Just jumpInsn
insn 5 = Just outputInsn
insn 23 = Just inputInsn
insn 39 = Just rotateInsn
insn 40 = Just derefInsn
insn 62 = Just opInsn
insn 68 = Just execNext
insn 81 = Just (const (const []))
insn _ = Nothing

jumpInsn :: State -> String -> String
jumpInsn (State a _ d mem) inp = execNext (State a (mem!d) d mem) inp

outputInsn :: State -> String -> String
outputInsn st@(State a _ _ _) inp = chr (a `mod` 256) : execNext st inp

inputInsn :: State -> String -> String
inputInsn (State _ c d mem) [] = execNext (State (3^10 - 1) c d mem) []
inputInsn (State _ c d mem) (ch:chs) = execNext (State (ord ch) c d mem) chs

rotateInsn :: State -> String -> String
rotateInsn (State _ c d mem) inp = execNext (State r c d (mem // [(d,r)])) inp
  where r = rotate (mem!d)

derefInsn :: State -> String -> String
derefInsn (State a c d mem) inp = execNext (State a c (mem!d) mem) inp

opInsn :: State -> String -> String
opInsn (State a c d mem) inp = execNext (State r c d (mem // [(d,r)])) inp
  where r = op a (mem!d)

execNext :: State -> String -> String
execNext (State a c d mem) =
    exec (State a (c + 1 `mod` 3^10) (d + 1 `mod` 3^10) (mem // [(c,m)]))
  where
    m | mem!c < 33 || mem!c > 126 = mem!c
      | otherwise = table!((mem!c + c - 33) `mod` 94)

exec :: State -> String -> String
exec state@(State a c d mem) input
  | mem!c < 33 || mem!c > 126 = []
  | otherwise = maybe (execNext state input)
                      (($ input) . ($ state))
                      (insn ((mem!c + c) `mod` 94))

initMem :: String -> State
initMem prog = State 0 0 0 (listArray (0,3^10-1) (fillMem 0 0 0 prog))
  where
    fillMem _ m1 m2 [] = (m1 `op` m2) : fillMem 0 (m1 `op` m2) m1 []
    fillMem i m1 m2 (c:cs)
      | isSpace c = fillMem i m1 m2 cs
      | ord c < 33 || ord c > 126 = error "invalid character in source file:"
      | otherwise =
            maybe (error "invalid character in source file:")
                  (const (ord c : fillMem (i + 1) (ord c) m1 cs))
                  (insn ((ord c + i) `mod` 94))

malbolge :: String -> IO ()
malbolge prog = (interact . exec . initMem) prog
