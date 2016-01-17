import Control.Monad(void)
import System.Environment(getArgs,getProgName)

import Compile(Compile,compile)
import Parse(parse)
import Check(check)
import Runtime(annotateRuntime)
import LowLevel(Func(..),FuncSig(..),toLowLevel)
import InterpRuntime(InterpRuntimeType,InterpRuntimeFunc)
import Memory(newMemory)
import Interp(callFunc)

type F = Func InterpRuntimeType InterpRuntimeFunc

main :: IO ()
main = getArgs >>= blo

blo :: [String] -> IO ()
blo [filename] = do
    source <- readFile filename
    either (putStrLn . show) runCode (compile (compileCode filename source))
blo _ = do
    progName <- getProgName
    putStrLn "Usage:"
    putStrLn (progName ++ " SRCFILE")

compileCode :: String -> String -> Compile (String -> Maybe F)
compileCode filename source =
    parse filename source >>= check
                          >>= fmap (flip lookup . toLowLevel) . annotateRuntime

runCode :: (String -> Maybe F) -> IO ()
runCode fns = run (fns "main")
  where
    run (Just func@(Func (FuncSig [] _) _)) = void (callFunc newMemory [] func)
    run _ = putStrLn "No zero-argument main func"
