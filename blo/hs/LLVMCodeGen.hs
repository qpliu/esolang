module LLVMCodeGen
    (codeGen)
where

import Control.Monad(foldM,unless,void,when,zipWithM_)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map

import LLVMGen
    (LLVMGen,Label,Temp,
     newTemp,newLabel,forwardRef,forwardRefTemp,forwardRefLabel,forwardRefInfo,
     writeNewTemp,writeNewLabel,writeCode,
     writeRefCountType,writeOffsetType,writeRTTOffsetType,
     writeTemp,writeLabel,writeLabelRef,writeName,writeBranch,
     gen)
import LLVMRuntime(LLVMRuntimeType(..),LLVMRuntimeFunc(..))
import LowLevel
    (Type(..),Func(..),FuncSig(..),Stmt(..),Expr(..),InsnId,
     funcMaxAliases,funcType,stmtLeavingScope,stmtNext,stmtId,stmtNextId)

type LLVMFunc = Func (LLVMRuntimeType FwdScope) (LLVMRuntimeFunc FwdScope)
type LLVMType = Type (LLVMRuntimeType FwdScope)
type LLVMFuncSig = FuncSig (LLVMRuntimeType FwdScope) (LLVMRuntimeFunc FwdScope)
type LLVMStmt = Stmt (LLVMRuntimeType FwdScope) (LLVMRuntimeFunc FwdScope)
type LLVMExpr = Expr (LLVMRuntimeType FwdScope) (LLVMRuntimeFunc FwdScope)

type Val = (Temp,Temp,Maybe (Temp,Temp),LLVMType)
type Scope = (Map String Val)
newtype FwdScope = FwdScope Scope
type CodeGen a = LLVMGen FwdScope a

codeGen :: ([(String,LLVMType)],[(String,LLVMFunc)]) -> String
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
    maxRTTOffset = maximum (map (\ (_,Type _ rtt) -> length rtt) types)
    genCode = gen maxAliases maxOffset maxRTTOffset

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
    writeBuiltinCopy,
    writeBuiltinCopyRTT
    ] ++ map writeBuiltinAlloc [2..maxAliases]

codeGenFunc :: (String,LLVMFunc) -> CodeGen ()
codeGenFunc (_,ImportFunc _ (LLVMRuntimeFunc _ importFunc)) = importFunc
codeGenFunc (name,Func funcSig stmt) = writeFunc name funcSig stmt

writeRetType :: Maybe LLVMType -> CodeGen ()
writeRetType Nothing = writeCode "void"
writeRetType (Just (Type _ rtt)) = do
    writeCode "{"
    writeValueType
    writeCode "*,"
    writeOffsetType
    unless (null rtt) (do
        writeCode ",i8**,"
        writeRTTOffsetType)
    writeCode "}"

writeFunc :: String -> LLVMFuncSig -> LLVMStmt -> CodeGen ()
writeFunc name (FuncSig params retType) stmt = do
    writeCode "define "
    writeRetType retType
    writeCode " @"
    writeName name
    let paramScope =
            zipWith (\ index param -> fmap ((,) index) param) [0..] params
    writeCode "("
    zipWithM_ writeParam ("":repeat ",") paramScope
    writeRetParam (if null params then "" else ",") retType
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
        Map.empty
        ((Set.toList . Set.fromList . map (snd . fst . snd)) varParams)
    varAllocsList <- mapM (\ (varKey,((aliases,size),rttsize)) -> do
        allocItems <- (sequence . take aliases . repeat) (do
            allocPtr <- writeNewTemp
            writeCode "alloca "
            writeOffsetType
            writeCode ","
            writeOffsetType
            writeCode " "
            writeTemp (varAllocSizes Map.! size)
            writeRTTPtr <- if rttsize == 0
                then return (writeCode "null")
                else do
                    rttPtr <- writeNewTemp
                    writeCode "alloca i8*,"
                    writeRTTOffsetType
                    writeCode (show rttsize)
                    return (writeTemp rttPtr)
            aggregate <- writeNewTemp
            writeCode "insertvalue {i8*,i8**} undef,i8* "
            writeTemp allocPtr
            writeCode ",0"
            allocItem <- writeNewTemp
            writeCode "insertvalue {i8*,i8**} "
            writeTemp aggregate
            writeCode ",i8** "
            writeRTTPtr
            writeCode ",1"
            return (allocPtr,allocItem))
        return (varKey,(varAllocSizes Map.! size,allocItems)))
        varParams
    varAllocs <- foldM (\ allocs (varKey,(size,items)) -> do
        mapM_ (writeClearAlloca size . fst) items
        return (Map.insert varKey (size,map snd items) allocs))
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
                writeCode ("select i1 1,i8** %import" ++ show index
                                                      ++ ",i8** null")
                impOffset <- writeNewTemp
                writeCode "select i1 1,"
                writeRTTOffsetType
                writeCode (" %importoffset" ++ show index ++ ",")
                writeRTTOffsetType
                writeCode " 0"
                return (Just (imp,impOffset))
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

writeAllocItemType :: CodeGen ()
writeAllocItemType = do
    writeCode "{"
    writeValueType
    writeCode ",i8**}"

writeParam :: String -> (String,(Int,LLVMType)) -> CodeGen ()
writeParam comma (_,(index,Type _ rtt)) = do
    writeCode comma
    writeValueType
    writeCode ("* %value" ++ show index ++ ",")
    writeOffsetType
    writeCode (" %offset" ++ show index)
    unless (null rtt) (do
        writeCode (",i8** %import" ++ show index ++ ",")
        writeRTTOffsetType
        writeCode (" %importoffset" ++ show index))

writeRetParam :: String -> Maybe LLVMType -> CodeGen ()
writeRetParam comma retType = do
    maybe (return ()) (const (do
            writeCode comma
            writeAllocItemType
            writeCode " %retval"))
        retType

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

writeNewRTT :: Temp -> LLVMType -> CodeGen (Maybe Temp)
writeNewRTT allocItem (Type _ []) = return Nothing
writeNewRTT allocItem (Type _ rtt) = do
    imp <- writeNewTemp
    writeCode "extractvalue "
    writeAllocItemType
    writeCode " "
    writeTemp allocItem
    writeCode ",1"
    zipWithM_ (\ (LLVMRuntimeType _ newrtt _ _) i -> do
            rttval <- newrtt
            rttPtr <- writeNewTemp
            writeCode "getelementptr i8*,i8** "
            writeTemp imp
            writeCode ","
            writeRTTOffsetType
            writeCode (" " ++ show i)
            writeCode " store i8* "
            writeTemp rttval
            writeCode ",i8** "
            writeTemp rttPtr)
        rtt [0..]
    return (Just imp)

writeRTTAddRef :: Val -> CodeGen ()
writeRTTAddRef (_,_,Nothing,_) = return ()
writeRTTAddRef (_,_,Just (imp,impOffset),Type _ rtt) =
    zipWithM_ (\ (LLVMRuntimeType _ _ addref _) i -> do
            index <- writeNewTemp
            writeCode "add "
            writeRTTOffsetType
            writeCode (" " ++ show i ++ ",")
            writeTemp impOffset
            rttPtr <- writeNewTemp
            writeCode "getelementptr i8*,i8** "
            writeTemp imp
            writeCode ","
            writeRTTOffsetType
            writeCode " "
            writeTemp index
            rttval <- writeNewTemp
            writeCode "load i8*,i8** "
            writeTemp rttPtr
            addref rttval)
        rtt [0..]

writeRTTUnref :: Val -> CodeGen ()
writeRTTUnref (_,_,Nothing,_) = return ()
writeRTTUnref (_,_,Just (imp,impOffset),Type _ rtt) =
    zipWithM_ (\ (LLVMRuntimeType _ _ _ unref) i -> do
            index <- writeNewTemp
            writeCode "add "
            writeRTTOffsetType
            writeCode (" " ++ show i ++ ",")
            writeTemp impOffset
            rttPtr <- writeNewTemp
            writeCode "getelementptr i8*,i8** "
            writeTemp imp
            writeCode ","
            writeRTTOffsetType
            writeCode " "
            writeTemp index
            rttval <- writeNewTemp
            writeCode "load i8*,i8** "
            writeTemp rttPtr
            unref rttval)
        rtt [0..]

writeStmt :: Map InsnId (Temp,[Temp])
          -> (Label,Bool,Bool,Scope,
              Map InsnId [(Label -> CodeGen(),(Label,Scope))])
          -> LLVMStmt
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
    w (StmtReturn _ expr) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        -- this should be the last stmt in a StmtBlock
        undefined
    w (StmtSetClear _ bit expr) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        val@(value,offset,_,_) <- wExpr expr
        bitPtr <- writeNewBitPtr (Left value) (Left offset)
        writeCode (" store i1 " ++ (if bit then "1" else "0") ++ ",i1* ")
        writeTemp bitPtr
        writeRTTUnref val
        (updatedScope,fellThru) <- updateScope scope
        return (blockLabel,inLoop,fellThru,updatedScope,branchFroms)
    w (StmtAssign _ lhs@(ExprVar varName) rhs) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        lval <- wExpr lhs
        rval <- wExpr rhs
        writeAddRef rval
        writeRTTUnref rval
        writeUnref lval
        undefined
        (updatedScope,fellThru) <-
            updateScope (Map.insert varName undefined scope)
        return (blockLabel,inLoop,fellThru,updatedScope,branchFroms)
    w (StmtAssign _ lhs rhs) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        lval <- wExpr lhs
        rval <- wExpr rhs
        writeCopyValue lval rval
        writeRTTUnref rval
        writeRTTUnref lval
        (updatedScope,fellThru) <- updateScope scope
        return (blockLabel,inLoop,fellThru,updatedScope,branchFroms)
    w (StmtExpr _ expr@(ExprFunc _ func _ _)) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        retVal <- writeCallFunc expr
        maybe (return ()) writeRTTUnref retVal
        (updatedScope,fellThru) <- updateScope scope
        return (blockLabel,inLoop,fellThru,updatedScope,branchFroms)
    w (StmtExpr _ expr) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        val <- wExpr expr
        writeRTTUnref val
        (updatedScope,fellThru) <- updateScope scope
        return (blockLabel,inLoop,fellThru,updatedScope,branchFroms)

    wExpr (ExprVar varName) = do
        let val = scope Map.! varName
        writeRTTAddRef val
        return val
    wExpr expr@(ExprFunc _ func _ _)  = do
        let Just retType@(Type _ rtt) = funcType func
        Just retVal <- writeCallFunc expr
        return retVal
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
        newImp <- if null rtt
                then return Nothing
                else if impOffset == 0
                    then return imp
                    else do
                        let Just (baseImp,baseImpOffset) = imp
                        newImpOffset <- writeNewTemp
                        writeCode " add "
                        writeRTTOffsetType
                        writeCode (" " ++ show impOffset ++ ",")
                        writeTemp baseImpOffset
                        return (Just (baseImp,newImpOffset))
        return (value,newOffset,newImp,exprType)

    writeCallFunc (ExprFunc funcName func argExprs (maxAliases,insnId)) = do
        args <- mapM wExpr argExprs
        retAlloc <- maybe (return Nothing)
                          (const (fmap Just (writeGetAlloc insnId)))
                          (funcType func)
        retVal <- maybe (writeCode " " >> return Nothing)
                        (const (fmap Just writeNewTemp)) (funcType func)
        writeCode "call "
        writeRetType (funcType func)
        writeCode " @"
        writeName funcName
        writeCode "("
        comma <- foldM (\ comma (val,offs,imp,Type _ rtt) -> do
            writeCode comma
            writeValueType
            writeCode "* "
            writeTemp val
            writeCode ","
            writeOffsetType
            writeCode " "
            writeTemp offs
            maybe (return ()) (\ (impVal,impOffset) -> do
                writeCode ",i8** "
                writeTemp impVal
                writeCode ","
                writeRTTOffsetType
                writeCode " "
                writeTemp impOffset)
                imp
            return ",")
            "" args
        maybe (return ()) (\ retAlloc -> do
            writeCode comma
            writeAllocItemType
            writeCode " "
            writeTemp retAlloc)
            retAlloc
        writeCode ")"
        mapM_ writeRTTUnref args
        maybe (return Nothing) (\ retType@(Type _ rtt) -> do
                let Just ret = retVal
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
                        writeCode "extractvalue "
                        writeRetType (Just retType)
                        writeCode " "
                        writeTemp ret
                        writeCode ",2"
                        impOffset <- writeNewTemp
                        writeCode "extractvalue "
                        writeRetType (Just retType)
                        writeCode " "
                        writeTemp ret
                        writeCode ",3"
                        return (Just (imp,impOffset))
                return (Just (value,offset,imp,retType)))
            (funcType func)

    writeGetAlloc insnId = do
        let (allocSize,allocs) = varAllocs Map.! insnId
        alloc <- if length allocs == 1
            then return (head allocs)
            else do
                alloc <- writeNewTemp
                writeCode " call "
                writeValueType
                writeCode ("* @alloc" ++ show (length allocs) ++ "(")
                mapM_ (\ (comma,alloc) -> do
                    writeCode (comma ++ "i8* ")
                    writeTemp alloc) (zip ("":repeat ",") allocs)
                writeCode ")"
                return alloc
        when inLoop (writeClearAlloca alloc allocSize)
        return alloc

    writeCopyValue (destval,destoffset,destimp,Type bitsize rtt)
                   (srcval,srcoffset,srcimp,_) = do
        writeCode " call void @copy("
        writeValueType
        writeCode " "
        writeTemp srcval
        writeCode ","
        writeOffsetType
        writeCode " "
        writeTemp srcoffset
        writeCode ","
        writeValueType
        writeCode " "
        writeTemp destval
        writeCode ","
        writeOffsetType
        writeCode " "
        writeTemp destoffset
        writeCode ","
        writeOffsetType
        writeCode (" " ++ show bitsize ++ ")")
        when (length rtt > 0) (do
            let Just (destimpval,destimpoffset) = destimp
            let Just (srcimpval,srcimpoffset) = srcimp
            writeCode " call void @copyrtt(i8** "
            writeTemp srcimpval
            writeCode ","
            writeRTTOffsetType
            writeCode " "
            writeTemp srcimpoffset
            writeCode ",i8** "
            writeTemp destimpval
            writeCode ","
            writeRTTOffsetType
            writeCode " "
            writeTemp destimpoffset
            writeCode ","
            writeRTTOffsetType
            writeCode (" " ++ show (length rtt) ++ ")"))

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
    iterateRef <- forwardRef (\ ((newIndex,newLabel,_):_) -> do
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
    iterateRef (newIndex,continueLabel,Nothing)
    writeCode "add "
    writeOffsetType
    writeCode " 1,"
    writeTemp index
    writeCode " br label "
    writeLabelRef loop
    retLabel <- writeNewLabel
    retLabelRef retLabel
    writeCode " ret void }"

writeBuiltinCopyRTT :: CodeGen ()
writeBuiltinCopyRTT = do
    writeCode "define void @copyrtt(i8** %srcval,"
    writeRTTOffsetType
    writeCode " %srcoffset,i8** %destval,"
    writeRTTOffsetType
    writeCode " %destoffset,"
    writeRTTOffsetType
    writeCode " %size) {"
    entry <- writeNewLabel
    writeCode " br label "
    loopRef <- forwardRefLabel writeLabelRef
    loop <- writeNewLabel
    loopRef loop
    index <- writeNewTemp
    writeCode "phi "
    writeRTTOffsetType
    writeCode "[0,"
    writeLabelRef entry
    writeCode "],"
    iterateRef <- forwardRef (\ ((newIndex,newLabel,_):_) -> do
        writeCode "["
        writeTemp newIndex
        writeCode ","
        writeLabelRef newLabel
        writeCode "]")
    cmp <- writeNewTemp
    writeCode "icmp ult "
    writeRTTOffsetType
    writeCode " "
    writeTemp index
    writeCode ",%size"
    (continueLabelRef,retLabelRef) <- writeBranch cmp
    continueLabel <- writeNewLabel
    continueLabelRef continueLabel
    srcIndex <- writeNewTemp
    writeCode "add "
    writeRTTOffsetType
    writeCode " %srcoffset,"
    writeTemp index
    srcPtr <- writeNewTemp
    writeCode "getelementptr i8*,i8** %srcval,"
    writeRTTOffsetType
    writeCode " "
    writeTemp srcIndex
    srcVal <- writeNewTemp
    writeCode "load i8*,i8** "
    writeTemp srcPtr
    destIndex <- writeNewTemp
    writeCode "add "
    writeRTTOffsetType
    writeCode " %destoffset,"
    writeTemp index
    destPtr <- writeNewTemp
    writeCode "getelementptr i8*,i8** %destval,"
    writeRTTOffsetType
    writeCode " "
    writeTemp destIndex
    writeCode " store i8* "
    writeTemp srcVal
    writeCode ",i8** "
    writeTemp destPtr
    newIndex <- writeNewTemp
    iterateRef (newIndex,continueLabel,Nothing)
    writeCode "add "
    writeRTTOffsetType
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
    writeAllocItemType
    writeCode ("* @alloc" ++ show n ++ "(")
    zipWithM_ param ("":repeat ",") [0..n-1]
    writeCode ") {"
    writeNewLabel
    labelRef <- foldM writeAlloc (const (return ())) [0..n-1]
    label <- writeNewLabel
    labelRef label
    writeCode " ret "
    writeAllocItemType
    writeCode "* undef }"
  where
    param comma i = do
        writeCode comma
        writeCode ("{i8*,i8**} %a" ++ show i)
    writeAlloc labelRef i = do
        label <- writeNewLabel
        labelRef label
        rawPtr <- writeNewTemp
        writeCode ("extractvalue {i8*,i8**} %a" ++ show i ++ ",0")
        allocPtr <- writeNewTemp
        writeCode "bitcast i8* "
        writeTemp rawPtr
        writeCode " to "
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
        impPtr <- writeNewTemp
        writeCode ("extractvalue {i8*,i8**} %a" ++ show i ++ ",1")
        halfAllocItem <- writeNewTemp
        writeCode "insertvalue "
        writeAllocItemType
        writeCode " undef,"
        writeValueType
        writeCode "* "
        writeTemp allocPtr
        writeCode ",0"
        allocItem <- writeNewTemp
        writeCode "insertvalue "
        writeAllocItemType
        writeCode " "
        writeTemp halfAllocItem
        writeCode ",i8** "
        writeTemp impPtr
        writeCode ",1"
        writeCode " ret "
        writeAllocItemType
        writeTemp allocItem
        return falseLabelRef

varMaxAliasesAndSizes :: LLVMStmt -> [(InsnId,((Int,Int),Int))]
varMaxAliasesAndSizes (StmtBlock _ stmts) =
    concatMap varMaxAliasesAndSizes stmts
varMaxAliasesAndSizes stmt@(StmtVar _ _ (Type bitSize rtt) maxAliases expr) =
    (stmtId stmt,((maxAliases,bitSize),length rtt))
        : maybe [] exprMaxAliasesAndSizes expr
varMaxAliasesAndSizes (StmtIf _ expr ifBlock elseBlock) =
    exprMaxAliasesAndSizes expr ++ varMaxAliasesAndSizes ifBlock
                                ++ (maybe [] varMaxAliasesAndSizes elseBlock)
varMaxAliasesAndSizes (StmtFor _ stmt) = varMaxAliasesAndSizes stmt
varMaxAliasesAndSizes (StmtSetClear _ _ expr) = exprMaxAliasesAndSizes expr
varMaxAliasesAndSizes (StmtAssign _ lhs rhs) =
    exprMaxAliasesAndSizes lhs ++ exprMaxAliasesAndSizes rhs
varMaxAliasesAndSizes (StmtExpr _ expr) = exprMaxAliasesAndSizes expr
varMaxAliasesAndSizes _ = []

exprMaxAliasesAndSizes :: Expr rtt rtf -> [(InsnId,((Int,Int),Int))]
exprMaxAliasesAndSizes (ExprField _ _ _ expr) = exprMaxAliasesAndSizes expr
exprMaxAliasesAndSizes (ExprFunc _ (Func (FuncSig _ retType) _) exprs
                                 (maxAliases,insnId)) =
    concatMap exprMaxAliasesAndSizes exprs ++ maybe [] aliasesAndSizes retType
  where
    aliasesAndSizes (Type bitSize rtt) =
        [(insnId,((maxAliases,bitSize),length rtt))]
exprMaxAliasesAndSizes _ = []
