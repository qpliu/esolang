module LLVMCodeGen
    (codeGen)
where

import Control.Monad(unless,zipWithM_)
import qualified Control.Monad.State as State
import Data.Char(isAlphaNum,isAscii,ord)
import qualified Data.Set as S

import LLVMRuntime
    (LLVMRuntimeType(..),LLVMRuntimeFunc(..),
     importTypeDeclarations,importFuncDeclarations,importFuncCode)
import LowLevel
    (Type(..),Func(..),FuncSig(..),Stmt(..),Expr(..),
     stmtLeavingScope,stmtNext)

codeGen :: ([(String,Type LLVMRuntimeType)],
            [(String,Func LLVMRuntimeType LLVMRuntimeFunc)]) -> String
codeGen (types,funcs) =
    (concat . S.toList . S.fromList) (concatMap (typeDecls . snd) types ++
                                      concatMap (funcDecls . snd) funcs ++
                                      builtinDecls auxTypes)
        ++ concat (builtinDefns auxTypes maxAliases)
        ++ concatMap (codeGenFunc auxTypes) funcs
  where
    maxAliases = getMaxAliases funcs
    auxTypes = (getRefCountType maxAliases,getOffsetType types)

typeDecls :: Type LLVMRuntimeType -> [String]
typeDecls (Type _ rtt) = concatMap importTypeDeclarations rtt

funcDecls :: Func LLVMRuntimeType LLVMRuntimeFunc -> [String]
funcDecls (Func _ _) = []
funcDecls (ImportFunc _ rtf) = importFuncDeclarations rtf

getMaxAliases :: [(String,Func LLVMRuntimeType LLVMRuntimeFunc)] -> Int
getMaxAliases funcs = 4 -- undefined

getRefCountType :: Int -> String
getRefCountType maxAliases
  | maxAliases < 256 = "i8"
  | maxAliases < 65536 = "i16"
  | otherwise = "i32"

getOffsetType :: [(String,Type LLVMRuntimeType)] -> String
getOffsetType types
  | maxOffset < 256 = "i8"
  | maxOffset < 65536 = "i16"
  | otherwise = "i32"
  where
    maxOffset = maximum (map (\ (_,Type bitSize _) -> bitSize) types)

builtinDecls :: (String,String) -> [String]
builtinDecls auxTypes = [
    "declare void @llvm.memset.p0i8.i8(i8*,i8,i8,i32,i1)"
    ]

builtinDefns :: (String,String) -> Int -> [String]
builtinDefns auxTypes maxAliases = [
    genCode auxTypes writeBuiltinCopy
    ] ++ map (genCode auxTypes . writeBuiltinAlloc) [2..maxAliases]

codeGenFunc :: (String,String) -> (String,Func LLVMRuntimeType LLVMRuntimeFunc)
                               -> String
codeGenFunc _ (_,ImportFunc _ rtf) = importFuncCode rtf
codeGenFunc auxTypes (name,Func funcSig stmt) =
    genCode auxTypes (writeFunc name funcSig stmt)

newtype Temp = Temp Int
newtype Label = Label Int

data CodeGenState = CodeGenState (Temp,Label) (String,String) [String]

type CodeGen a = State.State CodeGenState a

newTemp :: CodeGen Temp
newTemp = do
    CodeGenState (Temp t,label) auxTypes code <- State.get
    State.put (CodeGenState (Temp (t+1),label) auxTypes code)
    return (Temp t)

newLabel :: CodeGen Label
newLabel = do
    CodeGenState (temp,Label l) auxTypes code <- State.get
    State.put (CodeGenState (temp,Label (l+1)) auxTypes code)
    return (Label l)

writeCode :: String -> CodeGen ()
writeCode newcode = do
    CodeGenState counters auxTypes code <- State.get
    State.put (CodeGenState counters auxTypes (newcode:code))

writeOffsetType :: CodeGen ()
writeOffsetType = do
    CodeGenState counters auxTypes@(_,offsetType) code <- State.get
    State.put (CodeGenState counters auxTypes (offsetType:code))

writeRefCountType :: CodeGen ()
writeRefCountType = do
    CodeGenState counters auxTypes@(refCountType,_) code <- State.get
    State.put (CodeGenState counters auxTypes (refCountType:code))

writeTemp :: Temp -> CodeGen ()
writeTemp (Temp t) = writeCode ("%" ++ show t)

writeLabel :: Label -> CodeGen ()
writeLabel (Label l) = writeCode ("l" ++ show l ++ ":")

writeLabelRef :: Label -> CodeGen ()
writeLabelRef (Label l) = writeCode ("%l" ++ show l)

genCode :: (String,String) -> CodeGen () -> String
genCode auxTypes gen =
    let CodeGenState _ _ code =
            State.execState gen (CodeGenState (Temp 0,Label 0) auxTypes [])
    in  concat (reverse code)

writeRetType :: Maybe (Type LLVMRuntimeType) -> CodeGen ()
writeRetType Nothing = writeCode "void"
writeRetType (Just (Type _ rtt)) = do
    writeCode "{{"
    writeRefCountType
    writeCode ",[0 x i1]}*,"
    writeOffsetType
    unless (null rtt) (writeCode (",[" ++ show (length rtt) ++ " x i8*]"))
    writeCode "}"

writeName :: String -> CodeGen ()
writeName "main" = writeCode "main"
writeName name = writeCode ("_" ++ concatMap escape name)
  where
    escape ch | isAscii ch && isAlphaNum ch = [ch]
              | otherwise = "_" ++ show (ord ch) ++ "_"

writeFunc :: String -> FuncSig LLVMRuntimeType LLVMRuntimeFunc
                    -> Stmt LLVMRuntimeType LLVMRuntimeFunc
                    -> CodeGen ()
writeFunc name (FuncSig params retType) stmt = do
    writeCode "define "
    writeRetType retType
    writeCode " @"
    writeName name
    let scope = zipWith (\ index param -> fmap ((,) index) param) [0..] params
    writeCode "("
    zipWithM_ writeParam ("":repeat ",") scope
    maybe (return ()) (writeRetParam (if null params then "" else ",")) retType
    writeCode ") {"
    -- undefined
    writeCode "}"

writeValueType :: CodeGen ()
writeValueType = do
    writeCode "{"
    writeRefCountType
    writeCode ",[0 x i1]}*"

writeParam :: String -> (String,(Int,Type LLVMRuntimeType)) -> CodeGen ()
writeParam comma (_,(index,Type _ rtt)) = do
    writeCode comma
    writeValueType
    writeCode (" %value" ++ show index ++ ",")
    writeOffsetType
    writeCode (" %offset" ++ show index)
    unless (null rtt)
        (writeCode (",[" ++ show (length rtt) ++ " x i8*] %import"
                         ++ show index))

writeRetParam :: String -> Type LLVMRuntimeType -> CodeGen ()
writeRetParam comma retType = do
    writeCode comma
    writeValueType
    writeCode " %retval"

writeBuiltinCopy :: CodeGen ()
writeBuiltinCopy = do
    undefined

writeBuiltinAlloc :: Int -> CodeGen ()
writeBuiltinAlloc n = do
    undefined
