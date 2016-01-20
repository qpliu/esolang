import Control.Monad(unless,void)
import System.Directory(removeFile)
import System.Environment(getArgs,getProgName)
import System.Exit(ExitCode(..),exitFailure)
import System.FilePath(replaceExtension)
import System.IO(stderr,hPutStrLn)
import System.Process(readProcessWithExitCode)

import Compile(Compile,compile)
import Parse(parse)
import Check(check)
import Runtime(RuntimeType,RuntimeFunc,annotateRuntime)
import LowLevel(Type,Func(..),FuncSig(..),toLowLevel)
import InterpRuntime(InterpRuntimeType,InterpRuntimeFunc)
import Memory(newMemory)
import Interp(callFunc)
import LLVMCodeGen(codeGen)

main :: IO ()
main = getArgs >>= blo

blo :: [String] -> IO ()
blo [filename] = do
    source <- readFile filename
    either (die . show) (interp . snd) (compile (compileCode filename source))
blo ["-S",filename] = do
    source <- readFile filename
    either (die . show) (putStrLn . codeGen)
           (compile (compileCode filename source))
blo ["-s",filename] = do
    source <- readFile filename
    either (die . show) ((>>= putStr) . llvm . codeGen)
           (compile (compileCode filename source))
blo ["-c",filename] = do
    source <- readFile filename
    either (die . show) (llvmc (replaceExtension filename ".o") . codeGen)
           (compile (compileCode filename source))
blo ["-o",exefile,filename] = do
    source <- readFile filename
    let objfile = replaceExtension filename ".o"
    either (die . show) ((>> gcc objfile exefile) . llvmc objfile . codeGen)
           (compile (compileCode filename source))
blo _ = do
    progName <- getProgName
    die ("Usage:\n" ++ progName ++ " SRCFILE\n"
                    ++ progName ++ " -S SRCFILE\n"
                    ++ progName ++ " -s SRCFILE\n"
                    ++ progName ++ " -c SRCFILE\n"
                    ++ progName ++ " -o EXECUTABLE SRCFILE")

compileCode :: (RuntimeType rtt, RuntimeFunc rtf) =>
    String -> String -> Compile ([(String,Type rtt)],[(String,Func rtt rtf)])
compileCode filename source =
    parse filename source >>= check >>= fmap toLowLevel . annotateRuntime

interp :: [(String,Func InterpRuntimeType InterpRuntimeFunc)] -> IO ()
interp fns = run (lookup "main" fns)
  where
    run (Just func@(Func (FuncSig [] _) _)) =
        void (callFunc newMemory [] func)
    run _ = die "No zero-argument main func"

objFile :: String -> FilePath
objFile filename = replaceExtension filename ".o"

llvm :: String -> IO String
llvm llvmcode = do
    (exitCode,out,err) <-
        readProcessWithExitCode "llc" ["-filetype=asm"] llvmcode
    unless (exitCode == ExitSuccess) (die err)
    return out

llvmc :: String -> String -> IO ()
llvmc objfile llvmcode = do
    (exitCode,_,err) <-
        readProcessWithExitCode "llc" ["-filetype=obj", "-o", objfile] llvmcode
    unless (exitCode == ExitSuccess) (die err)

gcc :: String -> String -> IO ()
gcc objfile exefile = do
    (exitCode,_,err) <-
        readProcessWithExitCode "gcc" ["-o", exefile, objfile] ""
    removeFile objfile
    unless (exitCode == ExitSuccess) (die err)

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure
