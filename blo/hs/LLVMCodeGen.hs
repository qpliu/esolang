module LLVMCodeGen
    (codeGen)
where

import Control.Monad(unless,zipWithM_)
import qualified Data.Set as Set

import LLVMGen
    (CodeGen,Label,Temp,
     newTemp,newLabel,
     writeCode,writeRefCountType,writeOffsetType,
     writeTemp,writeLabel,writeLabelRef,writeName,
     gen)
import LLVMRuntime(LLVMRuntimeType(..),LLVMRuntimeFunc(..))
import LowLevel
    (Type(..),Func(..),FuncSig(..),Stmt(..),Expr(..),
     stmtLeavingScope,stmtNext)

codeGen :: ([(String,Type LLVMRuntimeType)],
            [(String,Func LLVMRuntimeType LLVMRuntimeFunc)]) -> String
codeGen (types,funcs) =
    uniq (concatMap (map genCode . typeDecls . snd) types
            ++ concatMap (map genCode . funcDecls . snd) funcs
            ++ map genCode builtinDecls)
        ++ concatMap genCode (builtinDefns maxAliases)
        ++ concatMap (genCode . codeGenFunc) funcs
  where
    uniq = concat . Set.toList . Set.fromList
    maxOffset = maximum (map (\ (_,Type bitSize _) -> bitSize) types)
    maxAliases = maximum (map (funcMaxAliases . snd) funcs)
    funcMaxAliases (Func _ _ maxAliases) = maxAliases
    funcMaxAliases _ = 0
    genCode = gen maxAliases maxOffset

    typeDecls (Type _ rtt) = concatMap rttTypeDecls rtt
    rttTypeDecls (LLVMRuntimeType decls) = decls
    funcDecls (ImportFunc _ (LLVMRuntimeFunc decls _)) = decls
    funcDecls _ = []

builtinDecls :: [CodeGen ()]
builtinDecls = [
    writeCode "declare void @llvm.memset.p0i8.i8(i8*,i8,i8,i32,i1)"
    ]

builtinDefns :: Int -> [CodeGen ()]
builtinDefns maxAliases = [
    writeBuiltinCopy
    ] ++ map writeBuiltinAlloc [2..maxAliases]

codeGenFunc :: (String,Func LLVMRuntimeType LLVMRuntimeFunc) -> CodeGen ()
codeGenFunc (_,ImportFunc _ (LLVMRuntimeFunc _ importFunc)) = importFunc
codeGenFunc (name,Func funcSig stmt _) = writeFunc name funcSig stmt

writeRetType :: Maybe (Type LLVMRuntimeType) -> CodeGen ()
writeRetType Nothing = writeCode "void"
writeRetType (Just (Type _ rtt)) = do
    writeCode "{{"
    writeRefCountType
    writeCode ",[0 x i1]}*,"
    writeOffsetType
    unless (null rtt) (writeCode (",[" ++ show (length rtt) ++ " x i8*]"))
    writeCode "}"

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
    undefined
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
    writeCode "define void @copy("
    writeValueType
    writeCode " %srcval,"
    writeOffsetType
    writeCode " %srcoffset,"
    writeValueType
    writeCode " %destval,"
    writeOffsetType
    writeCode " %destoffset,"
    writeOffsetType
    writeCode " %bitsize) {"
    undefined

writeBuiltinAlloc :: Int -> CodeGen ()
writeBuiltinAlloc n = do
    writeCode "define "
    writeValueType
    writeCode (" @alloc" ++ show n ++ "(")
    zipWithM_ param ("":repeat ",") [0..n-1]
    writeCode ") { br label "
    label <- newLabel
    writeLabelRef label
    writeLabel label
    undefined
  where
    param comma i = do
        writeCode comma
        writeValueType
        writeCode (" %a" ++ show i)
