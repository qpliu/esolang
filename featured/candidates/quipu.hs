-- https://esolangs.org/wiki/Quipu

-- Build: ghc --make quipu
-- Usage: quipu SRC-FILE

-- Notes:
-- The specification is unclear how input is handled.
-- The cat example suggests that it sucks the entire input into a string
-- value.  The exponentiation example suggests that two different numerical
-- values can be read by two different input knots.  The reference
-- implementation reads a line, converting it into a number, and, if the
-- conversion fails, treats it as a string.  Thus, the cat example is likely
-- wrong.
--
-- The specification is does not say how to treat string values when doing
-- numerical operations.  The reference implementation throws an exception
-- despite the specification saying the two ways to get an exception during
-- execution are referencing a nonexistent thread or jumping to a nonexistent
-- thread.
--
-- The specification lists ;; as a knot, presumably evaluating to the value
-- of the preceding string or number.  The reference implementation indicates
-- that it is not a knot, and also clarifies that the number and string
-- constants spanning multiple rows are a single knot, and not multiple knots.
--
-- The specification implies that the code is self-modifying by saying that
-- knots merge with previously evaluated knots.  The reference implementation
-- indicates that the merging of knots means that the values of previously
-- evaluated knots are discarded from the value stack, and the code is never
-- modified.  The specification confusingly uses the term knot to refer
-- both to code and to values.

import Data.Array(Array,array,bounds,inRange,(!),(//))
import Data.Char(isDigit)
import System.Environment(getArgs)

type Value = Either String Integer

data Knot = KnotThread | KnotNumber Integer | KnotString String
          | KnotDelimiter | KnotInput | KnotOutput | KnotCurrentThread
          | KnotDup | KnotOp (Integer -> Integer -> Integer)
          | KnotJumpIf (Integer -> Bool) | KnotJump | KnotHalt

parse :: String -> Array Integer [Knot]
parse prog = array (0,fromIntegral (length threads - 1)) (zip [0..] threads)
  where
    src :: String
    src | take 1 prog == "\"" = drop 1 (dropWhile (/= '"') (drop 1 prog))
        | otherwise = prog

    srcLines :: [String]
    srcLines = dropWhile (== "") (lines src)

    threadCols :: [Int]
    threadCols = cols (zip [0..] (head srcLines))
      where
        cols [] = []
        cols ((_,' '):rest) = cols rest
        cols ((n,_):rest) = n:cols (drop 1 rest)

    threads :: [[Knot]]
    threads = map reverse (foldl parseLine (take (length threadCols) (repeat [])) srcLines)

    parseLine :: [[Knot]] -> String -> [[Knot]]
    parseLine knots line = zipWith (parseKnot line) threadCols knots

    parseKnot :: String -> Int -> [Knot] -> [Knot]
    parseKnot line col knots
      | knot == "" || knot == "  " = knots
      | knot == "[]" = KnotThread:knots
      | knot == "\\n" = knotString '\n' knots
      | knot == "\\t" = knotString '\t' knots
      | take 1 knot == "'" = knotString (head (tail knot)) knots
      | drop 1 knot == "#" = knotNumber 1000 (take 1 knot) knots
      | drop 1 knot == "%" = knotNumber  100 (take 1 knot) knots
      | drop 1 knot == "@" = knotNumber   10 (take 1 knot) knots
      | drop 1 knot == "&" = knotNumber    1 (take 1 knot) knots
      | knot == ";;" = KnotDelimiter:knots
      | knot == "\\/" = KnotInput:knots
      | knot == "/\\" = KnotOutput:knots
      | knot == "^^" = KnotCurrentThread:knots
      | knot == "##" = KnotDup:knots
      | knot == "++" = KnotOp (+):knots
      | knot == "--" = KnotOp (-):knots
      | knot == "**" = KnotOp (*):knots
      | knot == "//" = KnotOp div:knots
      | knot == "%%" = KnotOp mod:knots
      | knot == "==" = KnotJumpIf (== 0):knots
      | knot == "<<" = KnotJumpIf (< 0):knots
      | knot == "<=" = KnotJumpIf (<= 0):knots
      | knot == ">>" = KnotJumpIf (> 0):knots
      | knot == ">=" = KnotJumpIf (>= 0):knots
      | knot == "??" = KnotJump:knots
      | knot == "::" = KnotHalt:knots
      | otherwise = error line
      where knot = take 2 (drop col line)

    knotString :: Char -> [Knot] -> [Knot]
    knotString ch (KnotString s:knots) = KnotString (s++[ch]):knots
    knotString ch knots = KnotString [ch]:knots

    knotNumber :: Integer -> String -> [Knot] -> [Knot]
    knotNumber 1000 d (KnotNumber n:knots) = KnotNumber (10*n+1000*read d):knots
    knotNumber f d (KnotNumber n:knots) = KnotNumber (n+f*read d):knots
    knotNumber f d knots = KnotNumber (f*read d):knots

readValue :: String -> Value
readValue val
  | all isDigit val = Right (read val)
  | take 1 val == "-" && all isDigit (tail val) = Right (read val)
  | otherwise = Left val

evalThread :: Array Integer [Knot] -> Array Integer Value -> Integer -> [String] -> String
evalThread threads threadValues threadNumber input =
    evalKnots threads threadValues threadNumber (threads!threadNumber) [threadValues!threadNumber] input

evalKnots :: Array Integer [Knot] -> Array Integer Value -> Integer -> [Knot] -> [Value] -> [String] -> String
evalKnots threads threadValues threadNumber knots stack input = eval knots stack
  where
    jump newThreadNumber stack = evalThread threads (threadValues // (map ((,) threadNumber)) (take 1 stack)) newThreadNumber input
    eval [] stack
      | inRange (bounds threads) (threadNumber+1) = jump (threadNumber+1) stack
      | otherwise = ""
    eval (KnotThread:knots) (Right n:stack) = eval knots ((threadValues!n):stack)
    eval (KnotNumber n:knots) stack = eval knots (Right n:stack)
    eval (KnotString s:knots) stack = eval knots (Left s:stack)
    eval (KnotDelimiter:knots) stack = eval knots stack
    eval (KnotInput:knots) stack
      | null input = eval knots (Left "":stack)
      | otherwise = evalKnots threads threadValues threadNumber knots (readValue (head input):stack) (tail input)
    eval (KnotOutput:knots) stack@(val:_) = either id show val ++ eval knots stack
    eval (KnotCurrentThread:knots) stack = eval knots ((threadValues!threadNumber):stack)
    eval (KnotDup:knots) stack@(v:_) = eval knots (v:stack)
    eval (KnotOp op:knots) stack@(Right n1:Right n2:_) = eval knots (Right (op n2 n1):stack)
    eval (KnotJumpIf cond:knots) (Right n1:stack@(Right n2:_))
      | cond n2 = jump n1 stack
      | otherwise = eval knots stack
    eval (KnotJump:knots) (Right n:stack) = jump n stack
    eval (KnotHalt:_) stack = ""
    eval knots stack = error ("thread=" ++ show threadNumber ++ ",remaining knots=" ++ show (length knots) ++ ",stack=" ++ show stack)

run :: String -> String -> String
run prog input = evalThread threads (fmap (const (Right 0)) threads) 0 (lines input)
  where
    threads = parse prog

quipu :: String -> IO ()
quipu prog = interact (run prog)

main :: IO ()
main = getArgs >>= readFile . head >>= quipu
