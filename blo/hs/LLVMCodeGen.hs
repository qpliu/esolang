module LLVMCodeGen
    (codeGen)
where

import Control.Monad(foldM,unless,void,when,zipWithM_)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map

import LLVMGen
    (CodeGen,Label,Temp,
     newTemp,newLabel,forwardRef,forwardRefTemp,forwardRefLabel,
     writeNewTemp,writeNewLabel,writeCode,writeRefCountType,writeOffsetType,
     writeTemp,writeLabel,writeLabelRef,writeName,writeBranch,
     gen)
import LLVMRuntime(LLVMRuntimeType(..),LLVMRuntimeFunc(..))
import LowLevel
    (Type(..),Func(..),FuncSig(..),Stmt(..),Expr(..),InsnId,
     funcMaxAliases,funcType,stmtLeavingScope,stmtNext,stmtId,stmtNextId)

type Val = (Temp,Temp,Maybe Temp,Type LLVMRuntimeType)
type Scope = Map String Val

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
    genCode = gen maxAliases maxOffset

    typeDecls (Type _ rtt) = concatMap rttTypeDecls rtt
    rttTypeDecls (LLVMRuntimeType decls _ _ _) = decls
    funcDecls (ImportFunc _ (LLVMRuntimeFunc decls _)) = decls
    funcDecls _ = []

builtinDecls :: [CodeGen ()]
builtinDecls = [
    (do writeCode "declare void @llvm.memset.p0i8."
        writeOffsetType
        writeCode "(i8*,i8,"
        writeOffsetType
        writeCode ",i32,i1)")
    ]

builtinDefns :: Int -> [CodeGen ()]
builtinDefns maxAliases = [
    writeBuiltinCopy
    ] ++ map writeBuiltinAlloc [2..maxAliases]

codeGenFunc :: (String,Func LLVMRuntimeType LLVMRuntimeFunc) -> CodeGen ()
codeGenFunc (_,ImportFunc _ (LLVMRuntimeFunc _ importFunc)) = importFunc
codeGenFunc (name,Func funcSig stmt) = writeFunc name funcSig stmt

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
    let paramScope =
            zipWith (\ index param -> fmap ((,) index) param) [0..] params
    writeCode "("
    zipWithM_ writeParam ("":repeat ",") paramScope
    maybe (return ()) (writeRetParam (if null params then "" else ",")) retType
    writeCode ") {"
    entry <- writeNewLabel

    let varParams = varMaxAliasesAndSizes stmt
    varAllocSizes <- foldM (\ sizes size -> do
        bitPtr <- writeNewBitPtr (Right "null") (Right (show size))
        sizeVar <- writeNewTemp
        writeCode "ptrtoint i1* "
        writeTemp bitPtr
        writeCode " to "
        writeOffsetType
        return (Map.insert size sizeVar sizes))
        Map.empty ((Set.toList . Set.fromList . map (snd . snd)) varParams)
    varAllocsList <- mapM (\ (varKey,(aliases,size)) -> do
        allocItems <- (sequence . take aliases . repeat) (do
            allocPtr <- writeNewTemp
            writeCode "alloca "
            writeOffsetType
            writeCode ","
            writeOffsetType
            writeCode " "
            writeTemp (varAllocSizes Map.! size)
            return (allocPtr))
        return (varKey,(varAllocSizes Map.! size,allocItems)))
        varParams
    varAllocs <- foldM (\ allocs (varKey,(size,items)) -> do
        mapM_ (writeClearAlloca size) items
        return (Map.insert varKey (size,items) allocs))
        Map.empty varAllocsList

    scope <- foldM (\ vars (name,(index,varType@(Type _ rtt))) -> do
        value <- writeNewTemp
        writeCode "select i1 1,"
        writeValueType
        writeCode ("* %value" ++ show index ++ ",")
        writeValueType
        writeCode "* null"
        offset <- writeNewTemp
        writeCode "select i1 1,"
        writeOffsetType
        writeCode (" %offset" ++ show index ++ ",")
        writeOffsetType
        writeCode " 0"
        imp <- if null rtt
            then return Nothing
            else do
                imp <- writeNewTemp
                writeCode ("select i1 1, [" ++ show (length rtt)
                                            ++ " x i8*] %import" ++ show index)
                return (Just imp)
        writeAddRef (value,offset,imp,varType)
        return (Map.insert name (value,offset,imp,varType) vars))
        Map.empty paramScope

    writeStmt varAllocs (entry,False,True,scope,Map.empty) stmt
    writeCode " }"

writeClearAlloca :: Temp -> Temp -> CodeGen ()
writeClearAlloca ptr size = do
    writeCode " call void @llvm.memset.p0i8."
    writeOffsetType
    writeCode "(i8* "
    writeTemp ptr
    writeCode ",i8 0,"
    writeOffsetType
    writeCode " "
    writeTemp size
    writeCode ",i32 0,i1 0)"

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

writeNewBitPtr :: Either Temp String -> Either Temp String -> CodeGen Temp
writeNewBitPtr value index = do
    bitPtr <- writeNewTemp
    writeCode "getelementptr "
    writeValueType
    writeCode ","
    writeValueType
    writeCode "* "
    either writeTemp writeCode value
    writeCode ",i32 0,i32 1,"
    writeOffsetType
    writeCode " "
    either writeTemp writeCode index
    return bitPtr

writeAddRef :: Val -> CodeGen ()
writeAddRef var@(value,_,_,_) = do
    refCountPtr <- writeNewTemp
    writeCode "getelementptr "
    writeValueType
    writeCode ","
    writeValueType
    writeCode "* "
    writeTemp value
    writeCode ",i32 0,i32 0"
    oldRefCount <- writeNewTemp
    writeCode "load "
    writeRefCountType
    writeCode ","
    writeRefCountType
    writeCode "* "
    writeTemp refCountPtr
    newRefCount <- writeNewTemp
    writeCode "add "
    writeRefCountType
    writeCode " 1,"
    writeTemp oldRefCount
    writeCode " store "
    writeRefCountType
    writeCode " "
    writeTemp newRefCount
    writeCode ","
    writeRefCountType
    writeCode "* "
    writeTemp refCountPtr
    writeRTTAddRef var

writeUnref :: Val -> CodeGen ()
writeUnref var@(value,_,_,_) = do
    refCountPtr <- writeNewTemp
    writeCode "getelementptr "
    writeValueType
    writeCode ","
    writeValueType
    writeCode "* "
    writeTemp value
    writeCode ",i32 0,i32 0"
    oldRefCount <- writeNewTemp
    writeCode "load "
    writeRefCountType
    writeCode ","
    writeRefCountType
    writeCode "* "
    writeTemp refCountPtr
    newRefCount <- writeNewTemp
    writeCode "sub "
    writeRefCountType
    writeCode " "
    writeTemp oldRefCount
    writeCode ",1"
    writeCode " store "
    writeRefCountType
    writeCode " "
    writeTemp newRefCount
    writeCode ","
    writeRefCountType
    writeCode "* "
    writeTemp refCountPtr
    writeRTTUnref var

writeNewRTT :: Type LLVMRuntimeType -> CodeGen (Maybe Temp)
writeNewRTT (Type _ []) = return Nothing
writeNewRTT (Type _ rtt) = do
    rttlist <- mapM (\ (LLVMRuntimeType _ newrtt _ _) -> newrtt) rtt
    Right imp <- foldM (\ lastImp (rttval,i) -> do
        nextImp <- writeNewTemp
        writeCode ("insertvalue [" ++ show (length rttlist) ++ " x i8*] ")
        either writeCode writeTemp lastImp
        writeCode ",i8* "
        writeTemp rttval
        writeCode ("," ++ show i)
        return (Right nextImp)) (Left "undef") (zip rttlist [0..])
    return (Just imp)

writeRTTAddRef :: Val -> CodeGen ()
writeRTTAddRef (_,_,Nothing,_) = return ()
writeRTTAddRef (_,_,Just imp,Type _ rtt) =
    mapM_ (\ (LLVMRuntimeType _ _ addref _,i) -> do
        rttval <- writeNewTemp
        writeCode ("extractvalue [" ++ show (length rtt) ++ " x i8*] ")
        writeTemp imp
        writeCode ("," ++ show i)
        addref rttval) (zip rtt [0..])

writeRTTUnref :: Val -> CodeGen ()
writeRTTUnref (_,_,Nothing,_) = return ()
writeRTTUnref (_,_,Just imp,Type _ rtt) =
    mapM_ (\ (LLVMRuntimeType _ _ _ unref,i) -> do
        rttval <- writeNewTemp
        writeCode ("extractvalue [" ++ show (length rtt) ++ " x i8*] ")
        writeTemp imp
        writeCode ("," ++ show i)
        unref rttval) (zip rtt [0..])

writeStmt :: Map InsnId (Temp,[Temp])
          -> (Label,Bool,Bool,Scope,
              Map InsnId [(Label -> CodeGen(),(Label,Scope))])
          -> Stmt LLVMRuntimeType LLVMRuntimeFunc
          -> CodeGen (Label,Bool,Bool,Scope,
              Map InsnId [(Label -> CodeGen(),(Label,Scope))])
writeStmt varAllocs (blockLabel,inLoop,fellThru,scope,branchFroms) stmt =
    w stmt
  where
    sid = stmtId stmt
    nsid = stmtNextId stmt
    checkNewBlock
      | Map.member sid branchFroms = do
            newBlockLabel <- writeNewLabel
            mapM_ (($ newBlockLabel) . fst) (branchFroms Map.! sid)
            -- update scope, emit phi instructions
            return (newBlockLabel,scope,Map.delete sid branchFroms)
      | fellThru = return (blockLabel,scope,branchFroms)
      | otherwise = error "not fellThru and not br target"
    updateScope newScope = do
        undefined -- unref vars going out of scope
        maybe (do writeCode " ret void"
                  return (newScope,False))
              (const (return (newScope,True)))
              nsid

    w (StmtBlock _ stmts) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        foldM (writeStmt varAllocs)
              (blockLabel,inLoop,True,scope,branchFroms) stmts
    w (StmtVar _ varName varType maxAliases expr) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        undefined
        (updatedScope,fellThru) <-
            updateScope (Map.insert varName undefined scope)
        return (blockLabel,inLoop,fellThru,updatedScope,branchFroms)
    w (StmtIf _ expr ifBlock elseBlock) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        undefined
    w (StmtFor _ stmt) = do
        newBlockLabel <- writeNewLabel
        -- setup forward ref for finalScope
        -- generate new scope from forward refs and predecessors
        (finalBlockLabel,_,finalFellThru,finalScope,branchFroms) <-
            writeStmt varAllocs (newBlockLabel,True,True,scope,branchFroms)
                      stmt
        if finalFellThru
            then do
                -- send Just finalScope back
                writeCode " br label "
                writeLabel newBlockLabel
            else do
                -- send Nothing back
                undefined
        return (finalBlockLabel,inLoop,False,finalScope,branchFroms)
    w (StmtBreak _) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        (updatedScope,fellThru) <- updateScope scope
        updatedBranchFroms <- if not fellThru
                then return branchFroms
                else do
                    writeCode " br label "
                    breakTargetRef <- forwardRefLabel writeLabelRef
                    -- save breakTargetRef
                    return branchFroms
        -- this should be the last stmt in a StmtBlock
        return (blockLabel,inLoop,False,updatedScope,updatedBranchFroms)
    w (StmtReturn _ stmt) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        -- this should be the last stmt in a StmtBlock
        undefined
    w (StmtSetClear _ bit expr) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        undefined
    w (StmtAssign _ lhs rhs) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        undefined
    w (StmtExpr _ expr) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        undefined

    wExpr (ExprVar varName) = do
        let val = scope Map.! varName
        writeRTTAddRef val
        return val
    wExpr expr@(ExprFunc _ func _ _)  = do
        let Just retType@(Type _ rtt) = funcType func
        Just ret <- writeCallFunc expr
        value <- writeNewTemp
        writeCode "extractvalue "
        writeRetType (Just retType)
        writeCode " "
        writeTemp ret
        writeCode ",0"
        offset <- writeNewTemp
        writeCode "extractvalue "
        writeRetType (Just retType)
        writeCode " "
        writeTemp ret
        writeCode ",1"
        imp <- if null rtt
            then return Nothing
            else do
                imp <- writeNewTemp
                writeCode "extractValue "
                writeRetType (Just retType)
                writeCode " "
                writeTemp ret
                writeCode ",2"
                return (Just imp)
        return (value,offset,imp,retType)
    wExpr (ExprField bitOffset impOffset exprType@(Type _ rtt) expr) = do
        (value,offset,imp,_) <- wExpr expr
        newOffset <- if bitOffset == 0
            then return offset
            else do
                newOffset <- writeNewTemp
                writeCode "add "
                writeOffsetType
                writeCode (" " ++ show bitOffset ++ ",")
                writeTemp offset
                return newOffset
        newImp <- undefined
        return (value,newOffset,newImp,exprType)

    writeCallFunc (ExprFunc funcName func argExprs (maxAliases,insnId)) = do
        undefined

    writeGetAlloc insnId = do
        let (allocSize,allocs) = varAllocs Map.! insnId
        if length allocs == 1
            then do
                when inLoop (writeClearAlloca (head allocs) allocSize)
                return (head allocs)
            else do
                alloc <- writeNewTemp
                writeCode " call "
                writeValueType
                writeCode ("* @alloc" ++ show (length allocs) ++ "(")
                mapM_ (\ (comma,alloc) -> do
                    writeCode (comma ++ "i8* ")
                    writeTemp alloc) (zip ("":repeat ",") allocs)
                writeCode ")"
                writeClearAlloca alloc allocSize
                return alloc

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
    writeCode " %bitsize) {"
    entry <- writeNewLabel
    writeCode " br label "
    loopRef <- forwardRefLabel writeLabelRef
    loop <- writeNewLabel
    loopRef loop
    index <- writeNewTemp
    writeCode "phi "
    writeOffsetType
    writeCode "[0,"
    writeLabelRef entry
    writeCode "],"
    iterateRef <- forwardRef (\ ((newIndex,newLabel):_) -> do
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
    writeCode ",%bitsize"
    (continueLabelRef,retLabelRef) <- writeBranch cmp
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
    iterateRef (newIndex,continueLabel)
    writeCode "add "
    writeOffsetType
    writeCode " 1,"
    writeTemp index
    writeCode " br label "
    writeLabelRef loop
    retLabel <- writeNewLabel
    retLabelRef retLabel
    writeCode " ret void }"

writeBuiltinAlloc :: Int -> CodeGen ()
writeBuiltinAlloc n = do
    writeCode "define "
    writeValueType
    writeCode ("* @alloc" ++ show n ++ "(")
    zipWithM_ param ("":repeat ",") [0..n-1]
    writeCode ") {"
    writeNewLabel
    labelRef <- foldM writeAlloc (const (return ())) [0..n-1]
    label <- writeNewLabel
    labelRef label
    writeCode " ret "
    writeValueType
    writeCode "* null }"
  where
    param comma i = do
        writeCode comma
        writeCode ("i8* %a" ++ show i)
    writeAlloc labelRef i = do
        label <- writeNewLabel
        labelRef label
        allocPtr <- writeNewTemp
        writeCode ("bitcast i8* %a" ++ show i ++ " to ")
        writeValueType
        writeCode "*"
        ptr <- writeNewTemp
        writeCode "getelementptr "
        writeValueType
        writeCode ","
        writeValueType
        writeCode "* "
        writeTemp allocPtr
        writeCode ",i32 0,i32 0"
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
        (trueLabelRef,falseLabelRef) <- writeBranch cmp
        trueLabel <- writeNewLabel
        trueLabelRef trueLabel
        writeCode " ret "
        writeValueType
        writeCode "* "
        writeTemp allocPtr
        return falseLabelRef

varMaxAliasesAndSizes :: Stmt LLVMRuntimeType LLVMRuntimeFunc
                         -> [(InsnId,(Int,Int))]
varMaxAliasesAndSizes (StmtBlock _ stmts) =
    concatMap varMaxAliasesAndSizes stmts
varMaxAliasesAndSizes stmt@(StmtVar _ _ (Type bitSize _) maxAliases expr) =
    (stmtId stmt,(maxAliases,bitSize)) : maybe [] exprMaxAliasesAndSizes expr
varMaxAliasesAndSizes (StmtIf _ expr ifBlock elseBlock) =
    exprMaxAliasesAndSizes expr ++ varMaxAliasesAndSizes ifBlock
                                ++ (maybe [] varMaxAliasesAndSizes elseBlock)
varMaxAliasesAndSizes (StmtFor _ stmt) = varMaxAliasesAndSizes stmt
varMaxAliasesAndSizes (StmtSetClear _ _ expr) = exprMaxAliasesAndSizes expr
varMaxAliasesAndSizes (StmtAssign _ lhs rhs) =
    exprMaxAliasesAndSizes lhs ++ exprMaxAliasesAndSizes rhs
varMaxAliasesAndSizes (StmtExpr _ expr) = exprMaxAliasesAndSizes expr
varMaxAliasesAndSizes _ = []

exprMaxAliasesAndSizes :: Expr rtt rtf -> [(InsnId,(Int,Int))]
exprMaxAliasesAndSizes (ExprField _ _ _ expr) = exprMaxAliasesAndSizes expr
exprMaxAliasesAndSizes (ExprFunc _ (Func (FuncSig _ retType) _) exprs
                                 (maxAliases,insnId)) =
    concatMap exprMaxAliasesAndSizes exprs ++ maybe [] aliasesAndSizes retType
  where
    aliasesAndSizes (Type bitSize _) = [(insnId,(maxAliases,bitSize))]
exprMaxAliasesAndSizes _ = []
