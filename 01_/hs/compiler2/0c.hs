import Control.Monad(unless)
import System.Directory(removeFile,getTemporaryDirectory)
import System.Environment(getArgs,getProgName)
import System.Exit(ExitCode(..),exitFailure)
import System.FilePath(combine,dropExtensions,replaceExtension,takeBaseName)
import System.IO(stderr,hPutStrLn)
import System.Posix(getProcessID)
import System.Process(readProcessWithExitCode)

import Compile(compile)
import Parse(parse)
import Resolve(resolve)
import CodeGen(codeGen,genMain)

main :: IO ()
main = do
    args <- getArgs
    maybe usage zeroc (parseArgs args)

usage :: IO ()
usage = do
    progName <- getProgName
    let arguments = " [-m MAIN-FUNCTION] SOURCE-FILE [SOURCE-FILE...]"
    die ("Usage:\n" ++ progName ++ " -S" ++ arguments ++ "\n"
                    ++ progName ++ " -s" ++ arguments ++ "\n"
                    ++ progName ++ " -c" ++ arguments ++ "\n"
                    ++ progName ++ " -o EXECUTABLE-FILE" ++ arguments)

parseArgs :: [String] -> Maybe (String -> IO (),String,[String])
parseArgs ("-S":"-m":mainFunc:srcFiles@(_:_)) =
    Just (putStrLn,mainFunc,srcFiles)
parseArgs ("-s":"-m":mainFunc:srcFiles@(_:_)) =
    Just (outAsm,mainFunc,srcFiles)
parseArgs ("-c":"-m":mainFunc:srcFiles@(srcFile:_)) =
    Just (outObj (objFile srcFile),mainFunc,srcFiles)
parseArgs ("-o":exeFile:"-m":mainFunc:srcFiles@(srcFile:_)) =
    Just (outExe exeFile,mainFunc,srcFiles)
parseArgs ("-S":srcFiles@(srcFile:_)) =
    Just (putStrLn,getMainFunc srcFile,srcFiles)
parseArgs ("-s":srcFiles@(srcFile:_)) =
    Just (outAsm,getMainFunc srcFile,srcFiles)
parseArgs ("-c":srcFiles@(srcFile:_)) =
    Just (outObj (objFile srcFile),getMainFunc srcFile,srcFiles)
parseArgs ("-o":exeFile:srcFiles@(srcFile:_)) =
    Just (outExe exeFile,getMainFunc srcFile,srcFiles)
parseArgs _ = Nothing

getMainFunc :: String -> String
getMainFunc = dropExtensions . takeBaseName

objFile :: String -> String
objFile srcFile = replaceExtension srcFile ".o"

zeroc :: (String -> IO (),String,[String]) -> IO ()
zeroc (outResult,mainFunc,srcFiles) = do
    sourceCode <- mapM readFile srcFiles
    funcs <- either (die . show) return (compile (do
        partials <- mapM (uncurry parse) (zip srcFiles sourceCode)
        resolve (concat partials)))
    mainDefs <- maybe (die ("Function not defined:" ++ mainFunc))
                      return (lookup mainFunc funcs)
    outResult (codeGen funcs ++ genMain mainFunc mainDefs)

outAsm :: String -> IO ()
outAsm llvmcode = do
    asm <- llvm llvmcode
    putStr asm

outObj :: String -> String -> IO ()
outObj objfile llvmcode = do
    llvmc objfile llvmcode

outExe :: String -> String -> IO ()
outExe exefile llvmcode = do
    pid <- getProcessID
    objfile <- fmap (flip combine (show pid)) getTemporaryDirectory
    llvmc objfile llvmcode
    gcc objfile exefile
    removeFile objfile

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
    unless (exitCode == ExitSuccess) (die err)

die :: String -> IO a
die msg = do
    hPutStrLn stderr msg
    exitFailure
