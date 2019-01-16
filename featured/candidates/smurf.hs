-- https://esolangs.org/wiki/Smurf

-- Build: ghc --make smurf
-- Usage: smurf SRC-FILE

-- Notes:
-- The specification says o outputs the top of the stack.  It does not say
-- that the top of the stack is popped off.  The perl implementation pops
-- the top of the stack.  This implementation follows the perl implementation.

import Data.Char(isSpace)
import Data.Map(Map,empty,insert,lookup)
import Prelude hiding (lookup)
import System.Environment(getArgs)

interp :: String -> [String] -> Map String String -> [String] -> String
interp code stack vars input = exec code
  where
    exec "" = ""
    exec ('"':code) = pushConst code ""
    exec ('+':code)
      | length stack < 2 = underflow
      | otherwise = interp code ((head (tail stack) ++ head stack):drop 2 stack) vars input
    exec ('i':code)
      | null input = interp code stack vars input
      | otherwise = interp code (head input:stack) vars (tail input)
    exec ('o':code)
      | null stack = underflow
      | otherwise = head stack ++ interp code (tail stack) vars input
    exec ('p':code)
      | length stack < 2 = underflow
      | otherwise = interp code (drop 2 stack) (insert (head stack) (head (tail stack)) vars) input
    exec ('g':code)
      | null stack = underflow
      | otherwise = interp code (maybe "" id (lookup (head stack) vars) : tail stack) vars input
    exec ('h':code)
      | null stack = underflow
      | null (head stack) = "Roll out that special head\nThis is our favourite one"
      | otherwise = interp code (take 1 (head stack) : tail stack) vars input
    exec ('t':code)
      | null stack = underflow
      | null (head stack) = "I'm not done\nAnd I won't be till my head falls off"
      | otherwise = interp code (tail (head stack) : tail stack) vars input
    exec ('q':code)
      | null stack = underflow
      | otherwise = interp code (quotify (head stack) : tail stack) vars input
    exec ('x':code)
      | null stack = underflow
      | otherwise = interp (head stack) [] empty input
    exec (c:code)
      | isSpace c = interp code stack vars input
      | otherwise = "It's hard to understand me from the language I use\nThere's no word in English for my style"
    underflow = "When the indicator says you're out of gas\nShould you continue driving anyway?"
    pushConst "" accum = "I was just talking and someone interrupted\nOr was it a loud explosion?"
    pushConst ('\\':'"':code) accum = pushConst code ('"':accum)
    pushConst ('\\':'\\':code) accum = pushConst code ('\\':accum)
    pushConst ('\\':'n':code) accum = pushConst code ('\n':accum)
    pushConst ('"':code) accum = interp code (reverse accum:stack) vars input
    pushConst (c:code) accum = pushConst code (c:accum)
    quotify str = "\"" ++ concatMap quotifyChar str ++ "\""
    quotifyChar '"' = "\\\""
    quotifyChar '\\' = "\\\\"
    quotifyChar '\n' = "\\n"
    quotifyChar c = [c]

run :: String -> String -> String
run prog input = interp prog [] empty (lines input)

smurf :: String -> IO ()
smurf prog = interact (run prog)

main :: IO ()
main = getArgs >>= readFile . head >>= smurf
