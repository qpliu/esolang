-- https://esolangs.org/wiki/Emmental

-- Build: ghc --make emmental
-- Usage: emmental SRC-FILE

import Data.Char(chr,ord)
import Data.Map(Map,findWithDefault,fromList,insert)
import System.Environment(getArgs)

data State = State [Char] [Char] (Map Char Insn)
type Insn = (State -> String -> String) -> State -> String -> String

chain :: Insn -> Insn -> Insn
chain insn1 insn2 continue state input = insn1 (insn2 continue) state input

insn :: Map Char Insn -> Char -> Insn
insn defs c = findWithDefault nop c defs

push :: Char -> Insn
push ch continue (State stack queue defs) input =
    continue (State (ch:stack) queue defs) input

digitInsn :: Int -> Insn
digitInsn digit continue (State (c:stack) queue defs) input =
    continue (State (chr (ord c*10 + digit) : stack) queue defs) input

digitDef :: Int -> (Char,Insn)
digitDef digit = (chr (ord '0' + digit), digitInsn digit)

add :: Insn
add continue (State (c1:c2:stack) queue defs) input =
    continue (State (chr ((ord c1 + ord c2) `mod` 256) : stack) queue defs) input

sub :: Insn
sub continue (State (c1:c2:stack) queue defs) input =
    continue (State (chr ((ord c2 - ord c1) `mod` 256) : stack) queue defs) input

log2 :: Insn
log2 continue (State (c:stack) queue defs) input =
    continue (State ((chr . log2 . ord) c : stack) queue defs) input
  where
    log2 0 = 8
    log2 arg = l2 0 (arg `div` 2)
    l2 l 0 = l
    l2 l arg = l2 (l+1) (arg `div` 2)

output :: Insn
output continue (State (c:stack) queue defs) input =
    c : continue (State stack queue defs) input

input :: Insn
input continue (State stack queue defs) (c:input) =
    continue (State (c:stack) queue defs) input

enqueue :: Insn
enqueue continue (State (c:stack) queue defs) input =
    continue (State (c:stack) (queue ++ [c]) defs) input

dequeue :: Insn
dequeue continue (State stack (c:queue) defs) input =
    continue (State (c:stack) queue defs) input

dup :: Insn
dup continue (State (c:stack) queue defs) input =
    continue (State (c:c:stack) queue defs) input

def :: Insn
def continue (State (c:stack) queue defs) input =
    continue (State newStack queue (insert c prog defs)) input
  where
    (body,';':newStack) = break (== ';') stack
    prog = foldl chain nop (map (insn defs) (reverse body))

eval :: Insn
eval continue (State (c:stack) queue defs) input =
    (insn defs c) continue (State stack queue defs) input

nop :: Insn
nop continue state input = continue state input

initialState :: State
initialState = State [] [] (fromList (map digitDef [0..9] ++ [
    ('#', push (chr 0)),
    ('+', add),
    ('-', sub),
    ('~', log2),
    ('.', output),
    (',', input),
    ('^', enqueue),
    ('v', dequeue),
    (':', dup),
    ('!', def),
    ('?', eval),
    (';', push ';')]))

interp :: String -> State -> String -> String
interp [] _ _ = []
interp (c:prog) state@(State _ _ defs) input =
    (insn defs c) (interp prog) state input

emmental :: String -> IO ()
emmental prog = interact (interp prog initialState)

main :: IO ()
main = getArgs >>= readFile . head >>= emmental
