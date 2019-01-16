-- https://esolangs.org/wiki/Brainfuck

-- Build: ghc --make brainfuck
-- Usage: brainfuck SRC-FILE

import Data.Char(chr,ord)
import System.Environment(getArgs)

bf :: Integral cell => (String,String) -> [cell] -> [cell] -> Maybe cell -> String -> String
bf ([],_) _ _ _ _ = []
bf ('>':prog,rprog) (cell:tape) rtape eof input =
    bf (prog,'>':rprog) tape (cell:rtape) eof input
bf ('<':prog,rprog) tape (cell:rtape) eof input =
    bf (prog,'<':rprog) (cell:tape) rtape eof input
bf ('+':prog,rprog) (cell:tape) rtape eof input =
    bf (prog,'+':rprog) (succ cell:tape) rtape eof input
bf ('-':prog,rprog) (cell:tape) rtape eof input =
    bf (prog,'-':rprog) (pred cell:tape) rtape eof input
bf ('.':prog,rprog) (cell:tape) rtape eof input =
    chr (fromIntegral cell) : bf (prog,'.':rprog) (cell:tape) rtape eof input
bf (',':prog,rprog) (_:tape) rtape eof (ch:input) =
    bf (prog,',':rprog) (fromIntegral (ord ch):tape) rtape eof input
bf (',':prog,rprog) (_:tape) rtape eof@(Just cell) [] =
    bf (prog,',':rprog) (cell:tape) rtape eof []
bf ('[':prog,rprog) (0:tape) rtape eof input =
    bf (jump (prog,'[':rprog)) (0:tape) rtape eof input
bf (']':prog,rprog) tape rtape eof input =
    bf (jumpback (']':prog,rprog)) tape rtape eof input
bf (insn:prog,rprog) tape rtape eof input = bf (prog,insn:rprog) tape rtape eof input

jump :: (String,String) -> (String,String)
jump ('[':prog,rprog) = jump (jump (prog,'[':rprog))
jump (']':prog,rprog) = (prog,']':rprog)
jump (insn:prog,rprog) = jump (prog,insn:rprog)
jump p = p

jumpback :: (String,String) -> (String,String)
jumpback (prog,'[':rprog) = ('[':prog,rprog)
jumpback (prog,']':rprog) = jumpback (jumpback (']':prog,rprog))
jumpback (prog,insn:rprog) = jumpback (insn:prog,rprog)
jumpback p = p

brainfuck :: String -> IO ()
brainfuck prog = interact (bf (prog,"") (replicate 30000 0) [] Nothing)

main :: IO ()
main = getArgs >>= readFile . head >>= brainfuck
