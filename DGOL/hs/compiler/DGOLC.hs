import Control.Exception(Exception,catch,displayException,throwIO)
import Control.Monad(foldM)
import qualified Data.ByteString
import Data.String(fromString)
import System.Directory(removeFile,getTemporaryDirectory)
import System.Environment(getArgs,getProgName)
import System.Exit(exitFailure)
import System.FilePath(combine,replaceExtension,takeExtension,takeFileName)
import System.Posix(getProcessID)
import System.Process(callProcess)

import qualified LLVM.AST
import LLVM.Context(Context,withContext)
import LLVM.IRBuilder.Module(ModuleBuilder,buildModule)
import LLVM.Module(File(File),Module,moduleLLVMAssembly,withModuleFromAST,withModuleFromBitcode,withModuleFromLLVMAssembly,writeLLVMAssemblyToFile,writeBitcodeToFile,writeTargetAssemblyToFile,writeObjectToFile)
import LLVM.Target(TargetMachine,withHostTargetMachine)

import qualified AST.AST
import AST.Parse(parse)
import CodeGen.CodeGen(codeGen)

newtype CompileException = CompileException String deriving Show

instance Exception CompileException where
    displayException (CompileException msg) = msg

dgolFileToModule :: [String] -> Context -> String -> (Module -> IO a) -> IO a
dgolFileToModule libs context file process = do
    src <- readFile file
    ast <- either (throwIO . CompileException . (file++) . (':':)) return $ parse file src
    let mbuilder = codeGen ast libs
    let mod = buildModule (fromString $ takeFileName file) mbuilder
    withModuleFromAST context mod { LLVM.AST.moduleSourceFileName = fromString file } process

llFileToModule :: Context -> String -> (Module -> IO a) -> IO a
llFileToModule context file process =
    withModuleFromLLVMAssembly context (File (fromString file)) process

bcFileToModule :: Context -> String -> (Module -> IO a) -> IO a
bcFileToModule context file process =
    withModuleFromBitcode context (File (fromString file)) process


moduleToLL :: String -> Module -> IO ()
moduleToLL file m =
    writeLLVMAssemblyToFile (File file) m

moduleToBC :: String -> Module -> IO ()
moduleToBC file m =
    writeBitcodeToFile (File file) m

moduleToAsm :: String -> Module -> IO ()
moduleToAsm file m = do
    withHostTargetMachine (\ targetMachine ->
        writeTargetAssemblyToFile targetMachine (File file) m)

moduleToObj :: String -> Module -> IO ()
moduleToObj file m = do
    withHostTargetMachine (\ targetMachine ->
        writeObjectToFile targetMachine (File file) m)

usage :: IO a
usage = do
    progName <- getProgName
    putStrLn "Usage:"
    putStr progName
    putStrLn " (-o BINFILE | -c | -s | -ll | -bc) [-L=LIB ...] FILE FILE ..."
    putStrLn ""
    putStrLn "Example:"
    putStr progName
    putStrLn " -o HELLO -L=IO HELLO.DGOL"
    exitFailure

data OutputOption = BinFile String | ObjFile | AsmFile | LLFile | BCFile

parseArgs :: IO (OutputOption,[String],[String])
parseArgs = do
    args <- getArgs
    maybe usage return $ p Nothing [] [] args
  where
    p _ _ [] [] = Nothing
    p _ _ _ ["-o"] = Nothing
    p Nothing libs files ("-o":out:args) = p (Just $ BinFile out) libs files args
    p Nothing libs [] ("-c":args) = p (Just ObjFile) libs [] args
    p Nothing libs [] ("-s":args) = p (Just AsmFile) libs [] args
    p Nothing libs [] ("-ll":args) = p (Just LLFile) libs [] args
    p Nothing libs [] ("-bc":args) = p (Just BCFile) libs [] args
    p out libs files (('-':'L':'=':lib@_):args) = p out (libs ++ [lib]) files args
    p Nothing libs [] (file:args) = p (Just $ BinFile "a.out") libs [file] args
    p (Just _) _ _ ("-o":args) = Nothing
    p (Just _) _ _ ("-c":args) = Nothing
    p (Just _) _ _ ("-s":args) = Nothing
    p (Just _) _ _ ("-ll":args) = Nothing
    p (Just _) _ _ ("-bc":args) = Nothing
    p (Just out) libs files (arg:args) = p (Just out) libs (files ++ [arg]) args
    p (Just out) libs files [] = Just (out,libs,files)

processFiles :: OutputOption -> [String] -> [String] -> IO ()
processFiles (BinFile f) libs files = do
    pid <- getProcessID
    tmpDir <- getTemporaryDirectory
    (tmpObjFiles,objFiles,_) <- foldM (makeTmpObjFiles (combine tmpDir $ show pid) libs) ([],[],1) files
    callProcess "cc" (["-o",f] ++ tmpObjFiles ++ objFiles)
    mapM_ removeFile tmpObjFiles

processFiles outputOption libs files = do
    mapM_ (withContext . processFile outputOption libs) files

processFile :: OutputOption -> [String] -> String -> Context -> IO ()
processFile ObjFile libs file context
  | takeExtension file == ".DGOL" = dgolFileToModule libs context file $ moduleToObj (replaceExtension file ".o")
  | takeExtension file == ".ll" = llFileToModule context file $ moduleToObj (replaceExtension file ".o")
  | takeExtension file == ".bc" = bcFileToModule context file $ moduleToObj (replaceExtension file ".o")
  | otherwise = return ()
processFile AsmFile libs file context
  | takeExtension file == ".DGOL" = dgolFileToModule libs context file $ moduleToAsm (replaceExtension file ".s")
  | takeExtension file == ".ll" = llFileToModule context file $ moduleToAsm (replaceExtension file ".s")
  | takeExtension file == ".bc" = bcFileToModule context file $ moduleToAsm (replaceExtension file ".s")
  | otherwise = return ()
processFile BCFile libs file context
  | takeExtension file == ".DGOL" = dgolFileToModule libs context file $ moduleToBC (replaceExtension file ".bc")
  | otherwise = return ()
processFile LLFile libs file context
  | takeExtension file == ".DGOL" = dgolFileToModule libs context file $ moduleToLL (replaceExtension file ".ll")
  | otherwise = return ()

makeTmpObjFiles :: String -> [String] -> ([String],[String],Int) -> String -> IO ([String],[String],Int)
makeTmpObjFiles tmpFileBase libs (tmpObjFiles,objFiles,index) file =
    withContext mkTmpObj
  where
    mkTmpObj context
      | takeExtension file == ".DGOL" = do
            let tmpObjFile = tmpFileBase ++ "." ++ show index ++ ".o"
            dgolFileToModule libs context file $ moduleToObj tmpObjFile
            return (tmpObjFile:tmpObjFiles,objFiles,index+1)
      | takeExtension file == ".ll" = do
            let tmpObjFile = tmpFileBase ++ "." ++ show index ++ ".o"
            llFileToModule context file $ moduleToObj tmpObjFile
            return (tmpObjFile:tmpObjFiles,objFiles,index+1)
      | takeExtension file == ".bc" = do
            let tmpObjFile = tmpFileBase ++ "." ++ show index ++ ".o"
            bcFileToModule context file $ moduleToObj tmpObjFile
            return (tmpObjFile:tmpObjFiles,objFiles,index+1)
      | takeExtension file `elem` [".s",".o",".a"] =
            return (tmpObjFiles,file:objFiles,index+1)
      | otherwise =
            return (tmpObjFiles,objFiles,index+1)

compileError :: CompileException -> IO a
compileError e = do
    putStrLn $ displayException e
    exitFailure

main :: IO ()
main = do
    (outputOption,libs,files) <- parseArgs
    catch (processFiles outputOption libs files) compileError

ll :: [String] -> String -> IO ()
ll libs file = do
    withContext (\ context -> do
        dgolFileToModule libs context file (\ m -> do
            llcode <- moduleLLVMAssembly m
            Data.ByteString.putStr llcode))
