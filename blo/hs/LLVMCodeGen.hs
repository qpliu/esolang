module LLVMCodeGen
    (codeGen)
where

import Control.Monad(foldM,foldM_,unless,void,when,zipWithM_)
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
newtype FwdScope = FwdScope (Maybe (Label,Scope))
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
        writeOffsetType "(i8*,i8,"
        writeOffsetType ",i32,i1)")
    ]

builtinDefns :: Int -> [CodeGen ()]
builtinDefns maxAliases = [
    writeBuiltinCopy,
    writeBuiltinCopyRTT
    ] ++ map writeBuiltinAlloc [2..maxAliases]

codeGenFunc :: (String,LLVMFunc) -> CodeGen ()
codeGenFunc (_,ImportFunc _ (LLVMRuntimeFunc _ importFunc)) = importFunc
codeGenFunc (name,Func funcSig stmt) = writeFunc name funcSig stmt

writeFunc :: String -> LLVMFuncSig -> LLVMStmt -> CodeGen ()
writeFunc name (FuncSig params retType) stmt = do
    writeCode "define "
    writeRetType retType " @"
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
        sizeVar <- writeNewTemp "ptrtoint i1* "
        writeTemp bitPtr " to "
        writeOffsetType ""
        return (Map.insert size sizeVar sizes))
        Map.empty
        ((Set.toList . Set.fromList . map (snd . fst . snd)) varParams)
    varAllocsList <- mapM (\ (varKey,((aliases,size),rttsize)) -> do
        allocItems <- (sequence . take aliases . repeat) (do
            allocPtr <- writeNewTemp "alloca "
            writeOffsetType ","
            writeOffsetType " "
            writeTemp (varAllocSizes Map.! size) ""
            writeRTTPtr <- if rttsize == 0
                then return (writeCode . ("null" ++))
                else do
                    rttPtr <- writeNewTemp "alloca i8*,"
                    writeRTTOffsetType (" "++show rttsize)
                    return (writeTemp rttPtr)
            aggregate <- writeNewTemp "insertvalue {i8*,i8**} undef,i8* "
            writeTemp allocPtr ",0"
            allocItem <- writeNewTemp "insertvalue {i8*,i8**} "
            writeTemp aggregate ",i8** "
            writeRTTPtr ",1"
            return (allocPtr,allocItem))
        return (varKey,(varAllocSizes Map.! size,allocItems)))
        varParams
    varAllocs <- foldM (\ allocs (varKey,(size,items)) -> do
        mapM_ (flip writeClearAlloca size . fst) items
        return (Map.insert varKey (size,map snd items) allocs))
        Map.empty varAllocsList

    scope <- foldM (\ vars (name,(index,varType@(Type _ rtt))) -> do
        value <- writeNewTemp "select i1 1,"
        writeValueType ("* %value" ++ show index ++ ",")
        writeValueType "* null"
        offset <- writeNewTemp "select i1 1,"
        writeOffsetType (" %offset" ++ show index ++ ",")
        writeOffsetType " 0"
        imp <- if null rtt
            then return Nothing
            else do
                imp <-
                    writeNewTemp ("select i1 1,i8** %import" ++ show index
                                                             ++ ",i8** null")
                impOffset <- writeNewTemp "select i1 1,"
                writeRTTOffsetType (" %importoffset" ++ show index ++ ",")
                writeRTTOffsetType " 0"
                return (Just (imp,impOffset))
        writeAddRef (value,offset,imp,varType)
        return (Map.insert name (value,offset,imp,varType) vars))
        Map.empty paramScope

    writeStmt (length params) varAllocs (entry,False,True,scope,Map.empty) stmt
    writeCode " }"

writeRetType :: Maybe LLVMType -> String -> CodeGen ()
writeRetType Nothing code = writeCode ("void" ++ code)
writeRetType (Just (Type _ rtt)) code = do
    writeCode "{"
    writeValueType "*,"
    writeOffsetType ""
    unless (null rtt) (do
        writeCode ",i8**,"
        writeRTTOffsetType "")
    writeCode ("}" ++ code)

writeClearAlloca :: Temp -> Temp -> CodeGen ()
writeClearAlloca ptr size = do
    writeCode " call void @llvm.memset.p0i8."
    writeOffsetType "(i8* "
    writeTemp ptr ",i8 0,"
    writeOffsetType " "
    writeTemp size ",i32 0,i1 0)"

writeValueType :: String -> CodeGen ()
writeValueType code = do
    writeCode "{"
    writeRefCountType (",[0 x i1]}" ++ code)

writeAllocItemType :: String -> CodeGen ()
writeAllocItemType code = do
    writeCode ("{i8*,i8**}" ++ code)

writeParam :: String -> (String,(Int,LLVMType)) -> CodeGen ()
writeParam comma (_,(index,Type _ rtt)) = do
    writeCode comma
    writeValueType ("* %value" ++ show index ++ ",")
    writeOffsetType (" %offset" ++ show index)
    unless (null rtt) (do
        writeCode (",i8** %import" ++ show index ++ ",")
        writeRTTOffsetType (" %importoffset" ++ show index))

writeRetParam :: String -> Maybe LLVMType -> CodeGen ()
writeRetParam comma retType = do
    maybe (return ()) (const (do
            writeCode comma
            writeAllocItemType " %retval"))
        retType

writeNewBitPtr :: Either Temp String -> Either Temp String -> CodeGen Temp
writeNewBitPtr value index = do
    bitPtr <- writeNewTemp "getelementptr "
    writeValueType ","
    writeValueType "* "
    either (flip writeTemp "") writeCode value
    writeCode ",i32 0,i32 1,"
    writeOffsetType " "
    either (flip writeTemp "") writeCode index
    return bitPtr

writeAddRef :: Val -> CodeGen ()
writeAddRef var@(value,_,_,_) = do
    refCountPtr <- writeNewTemp "getelementptr "
    writeValueType ","
    writeValueType "* "
    writeTemp value ",i32 0,i32 0"
    oldRefCount <- writeNewTemp "load "
    writeRefCountType ","
    writeRefCountType "* "
    writeTemp refCountPtr ""
    newRefCount <- writeNewTemp "add "
    writeRefCountType " 1,"
    writeTemp oldRefCount " store "
    writeRefCountType " "
    writeTemp newRefCount ","
    writeRefCountType "* "
    writeTemp refCountPtr ""
    writeRTTAddRef var

writeUnref :: Val -> CodeGen ()
writeUnref var@(value,_,_,_) = do
    refCountPtr <- writeNewTemp "getelementptr "
    writeValueType ","
    writeValueType "* "
    writeTemp value ",i32 0,i32 0"
    oldRefCount <- writeNewTemp "load "
    writeRefCountType ","
    writeRefCountType "* "
    writeTemp refCountPtr ""
    newRefCount <- writeNewTemp "sub "
    writeRefCountType " "
    writeTemp oldRefCount ",1"
    writeCode " store "
    writeRefCountType " "
    writeTemp newRefCount ","
    writeRefCountType "* "
    writeTemp refCountPtr ""
    writeRTTUnref var

writeNewRTT :: Temp -> LLVMType -> CodeGen (Maybe Temp)
writeNewRTT allocItem (Type _ []) = return Nothing
writeNewRTT allocItem (Type _ rtt) = do
    imp <- writeNewTemp "extractvalue "
    writeAllocItemType " "
    writeTemp allocItem ",1"
    zipWithM_ (\ (LLVMRuntimeType _ newrtt _ _) i -> do
            rttval <- newrtt
            rttPtr <- writeNewTemp "getelementptr i8*,i8** "
            writeTemp imp ","
            writeRTTOffsetType (" " ++ show i)
            writeCode " store i8* "
            writeTemp rttval ",i8** "
            writeTemp rttPtr "")
        rtt [0..]
    return (Just imp)

writeRTTAddRef :: Val -> CodeGen ()
writeRTTAddRef (_,_,Nothing,_) = return ()
writeRTTAddRef (_,_,Just (imp,impOffset),Type _ rtt) =
    zipWithM_ (\ (LLVMRuntimeType _ _ addref _) i -> do
            index <- writeNewTemp "add "
            writeRTTOffsetType (" " ++ show i ++ ",")
            writeTemp impOffset ""
            rttPtr <- writeNewTemp "getelementptr i8*,i8** "
            writeTemp imp ","
            writeRTTOffsetType " "
            writeTemp index ""
            rttval <- writeNewTemp "load i8*,i8** "
            writeTemp rttPtr ""
            addref rttval)
        rtt [0..]

writeRTTUnref :: Val -> CodeGen ()
writeRTTUnref (_,_,Nothing,_) = return ()
writeRTTUnref (_,_,Just (imp,impOffset),Type _ rtt) =
    zipWithM_ (\ (LLVMRuntimeType _ _ _ unref) i -> do
            index <- writeNewTemp "add "
            writeRTTOffsetType (" " ++ show i ++ ",")
            writeTemp impOffset ""
            rttPtr <- writeNewTemp "getelementptr i8*,i8** "
            writeTemp imp ","
            writeRTTOffsetType " "
            writeTemp index ""
            rttval <- writeNewTemp "load i8*,i8** "
            writeTemp rttPtr ""
            unref rttval)
        rtt [0..]

writeStmt :: Int -> Map InsnId (Temp,[Temp])
                 -> (Label,Bool,Bool,Scope,
                     Map InsnId [(Label -> CodeGen(),(Label,Scope))])
                 -> LLVMStmt
                 -> CodeGen (Label,Bool,Bool,Scope,
                             Map InsnId [(Label -> CodeGen(),(Label,Scope))])
writeStmt nparams varAllocs (blockLabel,inLoop,fellThru,scope,branchFroms)
          stmt =
    w stmt
  where
    sid = stmtId stmt
    nsid = stmtNextId stmt
    checkNewBlock
      | Map.member sid branchFroms = do
            (newBlockLabel,newScope,newBranchFroms,finalScopeRef) <- newBlock
            finalScopeRef (FwdScope Nothing)
            return (newBlockLabel,newScope,newBranchFroms)
      | fellThru = return (blockLabel,scope,branchFroms)
      | otherwise = error "not fellThru and not br target"
    newBlock = do
        newBlockLabel <- writeNewLabel
        mapM_ (($ newBlockLabel) . fst)
              (maybe [] id (Map.lookup sid branchFroms))
        newScope <- mapM (\ (name,(_,_,_,varType@(Type _ rtt))) -> do
                value <- newTemp
                offset <- newTemp
                imp <- if null rtt
                    then return Nothing
                    else do
                        imp <- newTemp
                        impOffset <- newTemp
                        return (Just (imp,impOffset))
                return (name,(value,offset,imp,varType)))
            (Map.toList scope)
        let predScopes = maybe [(blockLabel,scope)] (map snd)
                               (Map.lookup sid branchFroms)
        lastScopeRef <- forwardRefInfo (\ (FwdScope lastScope) -> do
            mapM_ (\ (name,(value,offset,imp,_)) -> do
                    let phiargs = maybe id (:) lastScope predScopes
                    writeCode " "
                    writeTemp value " = phi "
                    writeCode "{"
                    writeRefCountType ",[0 x i1]}* "
                    writePhiArgs name phiargs (\ (value,_,_,_) -> value)
                    writeCode " "
                    writeTemp offset " = phi "
                    writeOffsetType " "
                    writePhiArgs name phiargs (\ (_,offset,_,_) -> offset)
                    maybe (return ()) (\ (imp,impOffset) -> do
                            writeCode " "
                            writeTemp imp " = phi i8** "
                            writePhiArgs name phiargs
                                (\ (_,_,Just (imp,_),_) -> imp)
                            writeCode " "
                            writeTemp impOffset " = phi "
                            writeRTTOffsetType " "
                            writePhiArgs name phiargs
                                (\ (_,_,Just (_,impOffset),_) -> impOffset))
                        imp)
                newScope)
        return (newBlockLabel,Map.fromList newScope,Map.delete sid branchFroms,
                lastScopeRef)
    writePhiArgs name phiargs getarg = do
        foldM_ (\ comma (predLabel,predScope) -> do
                writeCode (comma ++ "[")
                writeTemp (getarg (predScope Map.! name)) ","
                writeLabelRef predLabel
                writeCode "]"
                return ",")
            "" phiargs
    updateScope newScope afterReturn = do
        updatedScope <- foldM (\ scope (varName,varType) -> do
                writeUnref (scope Map.! varName)
                return (Map.delete varName scope))
            newScope (stmtLeavingScope stmt (stmtNext stmt))
        maybe (do unless afterReturn (writeCode " ret void")
                  return (updatedScope,False))
              (const (return (updatedScope,True)))
              nsid
    branchNext newBlockLabel newScope branchFroms = do
        (updatedScope,fellThru) <- updateScope newScope False
        if not fellThru
            then return (newBlockLabel,inLoop,False,updatedScope,branchFroms)
            else do
                let Just nextId = nsid
                writeCode " br label "
                labelRef <- forwardRefLabel writeLabelRef
                let newBranchFroms =
                        Map.alter
                            (\ branchFrom ->
                                let froms = maybe [] id branchFrom
                                in  Just ((labelRef,(newBlockLabel,
                                                     updatedScope))
                                          : froms))
                        nextId branchFroms
                return (newBlockLabel,inLoop,False,updatedScope,newBranchFroms)

    w (StmtBlock _ []) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        (updatedScope,fellThru) <- updateScope scope False
        return (blockLabel,inLoop,fellThru,updatedScope,branchFroms)
    w (StmtBlock _ stmts) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        foldM (writeStmt nparams varAllocs)
              (blockLabel,inLoop,True,scope,branchFroms) stmts
    w (StmtVar _ varName varType@(Type _ rtt) maxAliases expr) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        val <- maybe (do
                allocItem <- writeGetAlloc sid
                rawPtr <- writeNewTemp "extractvalue "
                writeAllocItemType " "
                writeTemp allocItem ",0"
                value <- writeNewTemp "bitcast i8* "
                writeTemp rawPtr " to "
                writeValueType "*"
                offset <- writeNewTemp "select i1 1,"
                writeOffsetType " 0,"
                writeOffsetType " 0"
                imp <- if null rtt
                    then return Nothing
                    else do
                        imp <- writeNewTemp "extractvalue "
                        writeAllocItemType " "
                        writeTemp allocItem ",1"
                        zipWithM_ (\ (LLVMRuntimeType _ rttinit _ _) i -> do
                                rttval <- rttinit
                                rttPtr <-
                                    writeNewTemp "getelementptr i8*,i8** "
                                writeTemp imp ","
                                writeRTTOffsetType (" " ++ show i)
                                writeCode " store i8* "
                                writeTemp rttval ",i8** "
                                writeTemp rttPtr "")
                            rtt [0..]
                        impOffset <- writeNewTemp "select i1 1,"
                        writeRTTOffsetType " 0,"
                        writeRTTOffsetType " 0"
                        return (Just (imp,impOffset))
                let val = (value,offset,imp,varType)
                writeAddRef val
                return val)
            (\ expr -> do
                val <- wExpr scope expr
                writeAddRef val
                writeRTTUnref val
                return val)
            expr
        (updatedScope,fellThru) <-
            updateScope (Map.insert varName val scope) False
        return (blockLabel,inLoop,fellThru,updatedScope,branchFroms)
    w (StmtIf _ expr ifBlock elseBlock) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        (condValue,condOffset,_,_) <- wExpr scope expr
        bitPtr <- writeNewBitPtr (Left condValue) (Left condOffset)
        bit <- writeNewTemp "load i1,i1* "
        writeTemp bitPtr ""
        (trueLabelRef,falseLabelRef) <- writeBranch bit
        trueLabel <- writeNewLabel
        trueLabelRef trueLabel
        (trueBlockLabel,_,trueFellThru,trueScope,branchFroms) <-
            writeStmt nparams varAllocs
                      (trueLabel,inLoop,True,scope,branchFroms) ifBlock
        branchFroms <- if not trueFellThru
            then return branchFroms
            else do
                (_,_,_,_,branchFroms) <-
                    branchNext trueBlockLabel trueScope branchFroms
                return branchFroms
        falseLabel <- writeNewLabel
        falseLabelRef falseLabel
        maybe (branchNext falseLabel scope branchFroms) (\ elseBlock -> do
                (falseBlockLabel,_,falseFellThru,falseScope,branchFroms) <-
                    writeStmt nparams varAllocs
                              (falseLabel,inLoop,True,scope,branchFroms)
                              elseBlock
                if not falseFellThru
                    then return (blockLabel,inLoop,False,scope,branchFroms)
                    else branchNext falseBlockLabel falseScope branchFroms)
            elseBlock
    w (StmtFor _ stmt) = do
        loopLabelRef <- if fellThru
            then do
                writeCode " br label "
                forwardRefLabel writeLabelRef
            else return (const (return ()))
        (blockLabel,scope,branchFroms,finalScopeRef) <- newBlock
        loopLabelRef blockLabel
        (finalBlockLabel,_,finalFellThru,finalScope,branchFroms) <-
            writeStmt nparams varAllocs
                      (blockLabel,True,True,scope,branchFroms) stmt
        branchFroms <- if finalFellThru
            then do
                finalScopeRef (FwdScope (Just (finalBlockLabel,finalScope)))
                writeCode " br label "
                writeLabelRef blockLabel
                return branchFroms
            else
                maybe (do
                        finalScopeRef (FwdScope Nothing)
                        return branchFroms)
                      (\ comeFroms -> do
                        finalScopeRef
                            (FwdScope (Just (finalBlockLabel,finalScope)))
                        mapM_ (($ blockLabel) . fst) comeFroms
                        return (Map.delete sid branchFroms))
                      (Map.lookup sid branchFroms)
        return (finalBlockLabel,inLoop,False,finalScope,branchFroms)
    w (StmtBreak _) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        -- this should be the last stmt in a StmtBlock
        branchNext blockLabel scope branchFroms
    w (StmtReturn _ expr) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        exprVal <- maybe (return Nothing) (fmap Just . (wExpr scope)) expr
        (updatedScope,_) <- updateScope scope True
        maybe (writeCode " ret void")
            (\ val@(value,offset,imp,retType@(Type bitSize rtt)) -> do
                labelRef <- foldM (\ labelRef paramIndex -> do
                        maybe (return ()) (\ labelRef -> do
                                label <- writeNewLabel
                                labelRef label)
                            labelRef
                        cmp <- writeNewTemp "icmp eq "
                        writeValueType ("* %value" ++ show paramIndex ++ ",")
                        writeTemp value ""
                        (trueLabelRef,falseLabelRef) <- writeBranch cmp
                        trueLabel <- writeNewLabel
                        trueLabelRef trueLabel
                        retval1 <- writeNewTemp "insertvalue "
                        writeRetType (Just retType) " undef,"
                        writeValueType "* "
                        writeTemp value ",0"
                        retval2 <- writeNewTemp "insertvalue "
                        writeRetType (Just retType) " "
                        writeTemp retval1 ","
                        writeOffsetType " "
                        writeTemp offset ",1"
                        retval <- maybe (return retval2)
                            (\ (imp,impOffset) -> do
                                retval3 <- writeNewTemp "insertvalue "
                                writeRetType (Just retType) " "
                                writeTemp retval2 ",i8** "
                                writeTemp imp ",2"
                                retval4 <- writeNewTemp "insertvalue "
                                writeRetType (Just retType) " "
                                writeTemp retval3 ","
                                writeRTTOffsetType " "
                                writeTemp impOffset ",3"
                                return retval4)
                            imp
                        writeCode " ret "
                        writeRetType (Just retType) " "
                        writeTemp retval ""
                        return (Just falseLabelRef))
                    Nothing [0..nparams - 1]
                maybe (return ()) (\ labelRef -> do
                        label <- writeNewLabel
                        labelRef label)
                    labelRef
                retAllocRawPtr <- writeNewTemp "extractvalue "
                writeAllocItemType " %retval,0"
                retAlloc <- writeNewTemp "bitcast i8* "
                writeTemp retAllocRawPtr " to "
                writeValueType "*"
                retImpAlloc <- maybe (return Nothing) (const (do
                        retImpAlloc <- writeNewTemp "extractvalue "
                        writeAllocItemType " %retval,1"
                        return (Just (retImpAlloc,error "retImpAllocOffset"))))
                    imp
                writeCopyValue
                    (retAlloc,error "retAllocOffset",retImpAlloc,retType)
                    val True
                retval1 <- writeNewTemp "insertvalue "
                writeRetType (Just retType) " undef,"
                writeValueType "* "
                writeTemp retAlloc ",0"
                retval2 <- writeNewTemp "insertvalue "
                writeRetType (Just retType) " "
                writeTemp retval1 ","
                writeOffsetType " 0,1"
                retval <- maybe (return retval2)
                    (\ (imp,impOffset) -> do
                        retval3 <- writeNewTemp "insertvalue "
                        writeRetType (Just retType) " "
                        writeTemp retval2 ",i8** "
                        writeTemp imp ",2"
                        retval4 <- writeNewTemp "insertvalue "
                        writeRetType (Just retType) " "
                        writeTemp retval3 ","
                        writeRTTOffsetType " 0,3"
                        return retval4)
                    imp
                writeCode " ret "
                writeRetType (Just retType) " "
                writeTemp retval "")
            exprVal
        -- this should be the last stmt in a StmtBlock
        return (blockLabel,inLoop,False,updatedScope,branchFroms)
    w (StmtSetClear _ bit expr) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        val@(value,offset,_,_) <- wExpr scope expr
        bitPtr <- writeNewBitPtr (Left value) (Left offset)
        writeCode (" store i1 " ++ (if bit then "1" else "0") ++ ",i1* ")
        writeTemp bitPtr ""
        writeRTTUnref val
        (updatedScope,fellThru) <- updateScope scope False
        return (blockLabel,inLoop,fellThru,updatedScope,branchFroms)
    w (StmtAssign _ lhs@(ExprVar varName) rhs) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        lval <- wExpr scope lhs
        rval <- wExpr scope rhs
        writeAddRef rval
        writeRTTUnref rval
        writeRTTUnref lval
        writeUnref lval
        (updatedScope,fellThru) <-
            updateScope (Map.insert varName rval scope) False
        return (blockLabel,inLoop,fellThru,updatedScope,branchFroms)
    w (StmtAssign _ lhs rhs) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        lval <- wExpr scope lhs
        rval <- wExpr scope rhs
        writeRTTUnref lval
        writeCopyValue lval rval False
        (updatedScope,fellThru) <- updateScope scope False
        return (blockLabel,inLoop,fellThru,updatedScope,branchFroms)
    w (StmtExpr _ expr@(ExprFunc _ func _ _)) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        retVal <- writeCallFunc scope expr
        maybe (return ()) writeRTTUnref retVal
        (updatedScope,fellThru) <- updateScope scope False
        return (blockLabel,inLoop,fellThru,updatedScope,branchFroms)
    w (StmtExpr _ expr) = do
        (blockLabel,scope,branchFroms) <- checkNewBlock
        val <- wExpr scope expr
        writeRTTUnref val
        (updatedScope,fellThru) <- updateScope scope False
        return (blockLabel,inLoop,fellThru,updatedScope,branchFroms)

    wExpr scope (ExprVar varName) = do
        let val = scope Map.! varName
        writeRTTAddRef val
        return val
    wExpr scope expr@(ExprFunc _ func _ _)  = do
        let Just retType@(Type _ rtt) = funcType func
        Just retVal <- writeCallFunc scope expr
        return retVal
    wExpr scope (ExprField bitOffset impOffset exprType@(Type _ rtt) expr) = do
        (value,offset,imp,_) <- wExpr scope expr
        newOffset <- if bitOffset == 0
            then return offset
            else do
                newOffset <- writeNewTemp "add "
                writeOffsetType (" " ++ show bitOffset ++ ",")
                writeTemp offset ""
                return newOffset
        newImp <- if null rtt
                then return Nothing
                else if impOffset == 0
                    then return imp
                    else do
                        let Just (baseImp,baseImpOffset) = imp
                        newImpOffset <- writeNewTemp " add "
                        writeRTTOffsetType (" " ++ show impOffset ++ ",")
                        writeTemp baseImpOffset ""
                        return (Just (baseImp,newImpOffset))
        return (value,newOffset,newImp,exprType)

    writeCallFunc scope
                  (ExprFunc funcName func argExprs (maxAliases,insnId)) = do
        args <- mapM (wExpr scope) argExprs
        retAlloc <- maybe (return Nothing)
                          (const (fmap Just (writeGetAlloc insnId)))
                          (funcType func)
        retVal <- maybe (writeCode " " >> return Nothing)
                        (const (fmap Just (writeNewTemp "")))
                        (funcType func)
        writeCode "call "
        writeRetType (funcType func) " @"
        writeName funcName
        writeCode "("
        comma <- foldM (\ comma (val,offs,imp,Type _ rtt) -> do
            writeCode comma
            writeValueType "* "
            writeTemp val ","
            writeOffsetType " "
            writeTemp offs ""
            maybe (return ()) (\ (impVal,impOffset) -> do
                writeCode ",i8** "
                writeTemp impVal ","
                writeRTTOffsetType " "
                writeTemp impOffset "")
                imp
            return ",")
            "" args
        maybe (return ()) (\ retAlloc -> do
            writeCode comma
            writeAllocItemType " "
            writeTemp retAlloc "")
            retAlloc
        writeCode ")"
        mapM_ writeRTTUnref args
        maybe (return Nothing) (\ retType@(Type _ rtt) -> do
                let Just ret = retVal
                value <- writeNewTemp "extractvalue "
                writeRetType (Just retType) " "
                writeTemp ret ",0"
                offset <- writeNewTemp "extractvalue "
                writeRetType (Just retType) " "
                writeTemp ret ",1"
                imp <- if null rtt
                    then return Nothing
                    else do
                        imp <- writeNewTemp "extractvalue "
                        writeRetType (Just retType) " "
                        writeTemp ret ",2"
                        impOffset <- writeNewTemp "extractvalue "
                        writeRetType (Just retType) " "
                        writeTemp ret ",3"
                        return (Just (imp,impOffset))
                return (Just (value,offset,imp,retType)))
            (funcType func)

    writeGetAlloc insnId = do
        let (allocSize,allocs) = varAllocs Map.! insnId
        allocItem <- if length allocs == 1
            then return (head allocs)
            else do
                alloc <- writeNewTemp "call "
                writeAllocItemType (" @alloc" ++ show (length allocs) ++ "(")
                zipWithM_ (\ comma alloc -> do
                        writeCode comma
                        writeAllocItemType " "
                        writeTemp alloc "")
                    ("":repeat ",") allocs
                writeCode ")"
                return alloc
        when inLoop (do
            rawPtr <- writeNewTemp "extractvalue "
            writeAllocItemType " "
            writeTemp allocItem ",0"
            writeClearAlloca rawPtr allocSize)
        return allocItem

    writeCopyValue (destval,destoffset,destimp,Type bitsize rtt)
                   (srcval,srcoffset,srcimp,_) zeroDestOffset = do
        writeCode " call void @copy("
        writeValueType "* "
        writeTemp srcval ","
        writeOffsetType " "
        writeTemp srcoffset ","
        writeValueType "* "
        writeTemp destval ","
        writeOffsetType " "
        if zeroDestOffset then writeCode "0," else writeTemp destoffset ","
        writeOffsetType (" " ++ show bitsize ++ ")")
        when (length rtt > 0) (do
            let Just (destimpval,destimpoffset) = destimp
            let Just (srcimpval,srcimpoffset) = srcimp
            writeCode " call void @copyrtt(i8** "
            writeTemp srcimpval ","
            writeRTTOffsetType " "
            writeTemp srcimpoffset ",i8** "
            writeTemp destimpval ","
            writeRTTOffsetType " "
            if zeroDestOffset
                then writeCode "0,"
                else writeTemp destimpoffset ","
            writeRTTOffsetType (" " ++ show (length rtt) ++ ")"))

writeBuiltinCopy :: CodeGen ()
writeBuiltinCopy = do
    writeCode "define void @copy("
    writeValueType "* %srcval,"
    writeOffsetType " %srcoffset,"
    writeValueType "* %destval,"
    writeOffsetType " %destoffset,"
    writeOffsetType " %bitsize) {"
    entry <- writeNewLabel
    writeCode " br label "
    loopRef <- forwardRefLabel writeLabelRef
    loop <- writeNewLabel
    loopRef loop
    index <- writeNewTemp "phi "
    writeOffsetType "[0,"
    writeLabelRef entry
    writeCode "],"
    iterateRef <- forwardRef (\ ((newIndex,newLabel,_):_) -> do
        writeCode "["
        writeTemp newIndex ","
        writeLabelRef newLabel
        writeCode "]")
    cmp <- writeNewTemp "icmp ult "
    writeOffsetType " "
    writeTemp index ",%bitsize"
    (continueLabelRef,retLabelRef) <- writeBranch cmp
    continueLabel <- writeNewLabel
    continueLabelRef continueLabel
    srcIndex <- writeNewTemp "add "
    writeOffsetType " %srcoffset,"
    writeTemp index ""
    srcPtr <- writeNewTemp "getelementptr "
    writeValueType ","
    writeValueType "* %srcval,i32 0,i32 1,"
    writeOffsetType " "
    writeTemp srcIndex ""
    srcBit <- writeNewTemp "load i1,i1* "
    writeTemp srcPtr ""
    destIndex <- writeNewTemp "add "
    writeOffsetType " %destoffset,"
    writeTemp index ""
    destPtr <- writeNewTemp "getelementptr "
    writeValueType ","
    writeValueType "* %destval,i32 0,i32 1,"
    writeOffsetType " "
    writeTemp destIndex " store i1 "
    writeTemp srcBit ",i1* "
    writeTemp destPtr ""
    newIndex <- writeNewTemp "add "
    iterateRef (newIndex,continueLabel,Nothing)
    writeOffsetType " 1,"
    writeTemp index " br label "
    writeLabelRef loop
    retLabel <- writeNewLabel
    retLabelRef retLabel
    writeCode " ret void }"

writeBuiltinCopyRTT :: CodeGen ()
writeBuiltinCopyRTT = do
    writeCode "define void @copyrtt(i8** %srcval,"
    writeRTTOffsetType " %srcoffset,i8** %destval,"
    writeRTTOffsetType " %destoffset,"
    writeRTTOffsetType " %size) {"
    entry <- writeNewLabel
    writeCode " br label "
    loopRef <- forwardRefLabel writeLabelRef
    loop <- writeNewLabel
    loopRef loop
    index <- writeNewTemp "phi "
    writeRTTOffsetType "[0,"
    writeLabelRef entry
    writeCode "],"
    iterateRef <- forwardRef (\ ((newIndex,newLabel,_):_) -> do
        writeCode "["
        writeTemp newIndex ","
        writeLabelRef newLabel
        writeCode "]")
    cmp <- writeNewTemp "icmp ult "
    writeRTTOffsetType " "
    writeTemp index ",%size"
    (continueLabelRef,retLabelRef) <- writeBranch cmp
    continueLabel <- writeNewLabel
    continueLabelRef continueLabel
    srcIndex <- writeNewTemp "add "
    writeRTTOffsetType " %srcoffset,"
    writeTemp index ""
    srcPtr <- writeNewTemp "getelementptr i8*,i8** %srcval,"
    writeRTTOffsetType " "
    writeTemp srcIndex ""
    srcVal <- writeNewTemp "load i8*,i8** "
    writeTemp srcPtr ""
    destIndex <- writeNewTemp "add "
    writeRTTOffsetType " %destoffset,"
    writeTemp index ""
    destPtr <- writeNewTemp "getelementptr i8*,i8** %destval,"
    writeRTTOffsetType " "
    writeTemp destIndex " store i8* "
    writeTemp srcVal ",i8** "
    writeTemp destPtr ""
    newIndex <- writeNewTemp "add "
    iterateRef (newIndex,continueLabel,Nothing)
    writeRTTOffsetType " 1,"
    writeTemp index " br label "
    writeLabelRef loop
    retLabel <- writeNewLabel
    retLabelRef retLabel
    writeCode " ret void }"

writeBuiltinAlloc :: Int -> CodeGen ()
writeBuiltinAlloc n = do
    writeCode "define "
    writeAllocItemType (" @alloc" ++ show n ++ "(")
    zipWithM_ param ("":repeat ",") [0..n-1]
    writeCode ") {"
    writeNewLabel
    writeCode " br label "
    labelRef <- forwardRefLabel writeLabelRef
    labelRef <- foldM writeAlloc labelRef [0..n-1]
    label <- writeNewLabel
    labelRef label
    writeCode " ret "
    writeAllocItemType " undef }"
  where
    param comma i = do
        writeCode comma
        writeAllocItemType (" %a" ++ show i)
    writeAlloc labelRef i = do
        label <- writeNewLabel
        labelRef label
        rawPtr <- writeNewTemp "extractvalue "
        writeAllocItemType (" %a" ++ show i ++ ",0")
        allocPtr <- writeNewTemp "bitcast i8* "
        writeTemp rawPtr " to "
        writeValueType "*"
        ptr <- writeNewTemp "getelementptr "
        writeValueType ","
        writeValueType "* "
        writeTemp allocPtr ",i32 0,i32 0"
        refCount <- writeNewTemp "load "
        writeRefCountType ","
        writeRefCountType "* "
        writeTemp ptr ""
        cmp <- writeNewTemp "icmp eq "
        writeRefCountType " 0,"
        writeTemp refCount ""
        (trueLabelRef,falseLabelRef) <- writeBranch cmp
        trueLabel <- writeNewLabel
        trueLabelRef trueLabel
        writeCode " ret "
        writeAllocItemType (" %a" ++ show i)
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
