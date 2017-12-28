import Control.Exception(Exception,catch,displayException,throwIO)
import qualified Data.ByteString
import Data.Either(lefts,rights)
import Data.String(fromString)
import System.Environment(getArgs,getProgName)
import System.Exit(exitFailure)

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

dgolFileToModule :: [String] -> Context -> String -> IO Module
dgolFileToModule libs context file = do
    src <- readFile file
    ast <- either (throwIO . CompileException) return $ parse src
    let mbuilder = codeGen ast libs
    let mod = buildModule (fromString $ basename file) mbuilder
    withModuleFromAST context mod { LLVM.AST.moduleSourceFileName = fromString file } return

llFileToModule :: Context -> String -> IO Module
llFileToModule context file =
    withModuleFromLLVMAssembly context (File (fromString file)) return

bcFileToModule :: Context -> String -> IO Module
bcFileToModule context file =
    withModuleFromBitcode context (File (fromString file)) return


moduleToLL :: String -> Module -> IO ()
moduleToLL file m =
    writeLLVMAssemblyToFile (File file) m

moduleToBC :: String -> Module -> IO ()
moduleToBC file m =
    writeBitcodeToFile (File file) m

moduleToAsm :: String -> Module -> IO ()
moduleToAsm file m = do
    targetMachine <- withHostTargetMachine return
    writeTargetAssemblyToFile targetMachine (File file) m

moduleToObj :: String -> Module -> IO ()
moduleToObj file m = do
    targetMachine <- withHostTargetMachine return
    writeObjectToFile targetMachine (File file) m


basename :: String -> String
basename file = reverse $ takeWhile (/= '/') $ reverse file

dirname :: String -> String
dirname file = reverse $ dropWhile (/= '/') $ reverse file

ext :: String -> String
ext file
  | '.' `elem` base = reverse $ takeWhile (/= '.') $ reverse base
  | otherwise = ""
  where base = basename file

replaceExt :: String -> String -> String
replaceExt file ext
  | '.' `elem` base = (dir ++) $ (++ ext) $ reverse $ dropWhile (/= '.') $ reverse base
  | otherwise = file ++ '.':ext
  where base = basename file
        dir = dirname file


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
    p out libs [] (('-':'L':'=':lib@_):args) = p out (libs ++ [lib]) [] args
    p Nothing libs [] (file:args) = p (Just $ BinFile "a.out") libs [file] args
    p (Just _) _ _ ("-o":args) = Nothing
    p (Just _) _ _ ("-c":args) = Nothing
    p (Just _) _ _ ("-s":args) = Nothing
    p (Just _) _ _ ("-ll":args) = Nothing
    p (Just _) _ _ ("-bc":args) = Nothing
    p (Just out) libs files (arg:args) = p (Just out) libs (files ++ [arg]) args
    p (Just out) libs files [] = Just (out,libs,files)

processFiles :: OutputOption -> [String] -> [String] -> Context -> IO ()
processFiles (BinFile f) libs files context = undefined
processFiles outputOption libs files context = do
    mapM_ (processFile outputOption context libs) files

processFile :: OutputOption -> Context -> [String] -> String -> IO ()
processFile ObjFile context libs file
  | ext file == "DGOL" = dgolFileToModule libs context file >>= moduleToObj (replaceExt file "o")
  | ext file == "ll" = llFileToModule context file >>= moduleToObj (replaceExt file "o")
  | ext file == "bc" = bcFileToModule context file >>= moduleToObj (replaceExt file "o")
  | otherwise = return ()
processFile AsmFile context libs file
  | ext file == "DGOL" = dgolFileToModule libs context file >>= moduleToAsm (replaceExt file "s")
  | ext file == "ll" = llFileToModule context file >>= moduleToAsm (replaceExt file "s")
  | ext file == "bc" = bcFileToModule context file >>= moduleToAsm (replaceExt file "s")
  | otherwise = return ()
processFile BCFile context libs file
  | ext file == "DGOL" = dgolFileToModule libs context file >>= moduleToBC (replaceExt file "bc")
  | otherwise = return ()
processFile LLFile context libs file
  | ext file == "DGOL" = dgolFileToModule libs context file >>= moduleToBC (replaceExt file "ll")
  | otherwise = return ()

main :: IO ()
main = do
    (outputOption,libs,files) <- parseArgs
    withContext $ processFiles outputOption libs files

ll :: [String] -> String -> IO ()
ll libs file = do
    withContext (\ context -> do
        m <- dgolFileToModule libs context file
        llcode <- moduleLLVMAssembly m
        Data.ByteString.putStr llcode)
