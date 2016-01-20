module LLVMCodeGen
    (codeGen)
where

import Control.Monad(foldM,unless,zipWithM_)
import qualified Data.Set as Set

import LLVMGen
    (CodeGen,Label,Temp,
     newTemp,newLabel,forwardRef,forwardRefTemp,forwardRefLabel,
     writeNewTemp,writeNewLabel,writeCode,writeRefCountType,writeOffsetType,
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
    --error "writeFunc undefined"
    writeCode "}"

writeValueType :: CodeGen ()
writeValueType = do
    writeCode "{"
    writeRefCountType
    writeCode ",[0 x i1]}"

writeParam :: String -> (String,(Int,Type LLVMRuntimeType)) -> CodeGen ()
writeParam comma (_,(index,Type _ rtt)) = do
    writeCode comma
    writeValueType
    writeCode ("* %value" ++ show index ++ ",")
    writeOffsetType
    writeCode (" %offset" ++ show index)
    unless (null rtt)
        (writeCode (",[" ++ show (length rtt) ++ " x i8*] %import"
                         ++ show index))

writeRetParam :: String -> Type LLVMRuntimeType -> CodeGen ()
writeRetParam comma retType = do
    writeCode comma
    writeValueType
    writeCode "* %retval"

writeBuiltinCopy :: CodeGen ()
writeBuiltinCopy = do
    writeCode "define void @copy("
    writeValueType
    writeCode "* %srcval,"
    writeOffsetType
    writeCode " %srcoffset,"
    writeValueType
    writeCode "* %destval,"
    writeOffsetType
    writeCode " %destoffset,"
    writeOffsetType
    writeCode " %bitsize) { entry: br label "
    refLoop <- forwardRefLabel writeLabelRef
    loop <- writeNewLabel
    refLoop loop
    index <- writeNewTemp
    writeCode "phi "
    writeOffsetType
    writeCode "[0,%entry],"
    refIterate <- forwardRef (\ ((newIndex,newLabel):_) -> do
        writeCode "["
        writeTemp newIndex
        writeCode ","
        writeLabelRef newLabel
        writeCode "]")
    cmp <- writeNewTemp
    writeCode "icmp ult "
    writeOffsetType
    writeCode " "
    writeTemp index
    writeCode ",%bitsize br i1 "
    writeTemp cmp
    writeCode ",label "
    continueLabelRef <- forwardRefLabel writeLabelRef
    writeCode ",label "
    refRetLabel <- forwardRefLabel writeLabelRef
    continueLabel <- writeNewLabel
    continueLabelRef continueLabel
    srcIndex <- writeNewTemp
    writeCode "add "
    writeOffsetType
    writeCode " %srcoffset,"
    writeTemp index
    srcPtr <- writeNewTemp
    writeCode "getelementptr "
    writeValueType
    writeCode ","
    writeValueType
    writeCode "* %srcval,i32 0,i32 1,"
    writeOffsetType
    writeCode " "
    writeTemp srcIndex
    srcBit <- writeNewTemp
    writeCode "load i1,i1* "
    writeTemp srcPtr
    destIndex <- writeNewTemp
    writeCode "add "
    writeOffsetType
    writeCode " %destoffset,"
    writeTemp index
    destPtr <- writeNewTemp
    writeCode "getelementptr "
    writeValueType
    writeCode ","
    writeValueType
    writeCode "* %destval,i32 0,i32 1,"
    writeOffsetType
    writeCode " "
    writeTemp destIndex
    writeCode " store i1 "
    writeTemp srcBit
    writeCode ",i1* "
    writeTemp destPtr
    newIndex <- writeNewTemp
    refIterate (newIndex,continueLabel)
    writeCode "add "
    writeOffsetType
    writeCode " 1,"
    writeTemp index
    writeCode " br label "
    writeLabelRef loop
    retLabel <- writeNewLabel
    refRetLabel retLabel
    writeCode " ret void }"

writeBuiltinAlloc :: Int -> CodeGen ()
writeBuiltinAlloc n = do
    writeCode "define "
    writeValueType
    writeCode ("* @alloc" ++ show n ++ "(")
    zipWithM_ param ("":repeat ",") [0..n-1]
    writeCode ") {"
    labelRef <- foldM writeAlloc (const (return ())) [0..n-1]
    label <- writeNewLabel
    labelRef label
    writeCode " ret "
    writeValueType
    writeCode "* null }"
  where
    param comma i = do
        writeCode comma
        writeValueType
        writeCode ("* %a" ++ show i)
    writeAlloc labelRef i = do
        label <- writeNewLabel
        labelRef label
        ptr <- writeNewTemp
        writeCode "getelementptr "
        writeValueType
        writeCode ","
        writeValueType
        writeCode ("* %a" ++ show i ++ ",i32 0,i32 0")
        refCount <- writeNewTemp
        writeCode "load "
        writeRefCountType
        writeCode "* "
        writeTemp ptr
        cmp <- writeNewTemp
        writeCode "icmp eq "
        writeRefCountType
        writeCode " 0,"
        writeTemp refCount
        writeCode " br i1 "
        writeTemp cmp
        writeCode ",label "
        trueLabelRef <- forwardRefLabel writeLabelRef
        writeCode ",label "
        falseLabelRef <- forwardRefLabel writeLabelRef
        trueLabel <- writeNewLabel
        trueLabelRef trueLabel
        writeCode " ret "
        writeValueType
        writeCode "* "
        writeTemp ptr
        return falseLabelRef
