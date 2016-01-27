module LLVMGen
    (LLVMGen,Label,Temp,
     newTemp,newLabel,forwardRef,forwardRefTemp,forwardRefLabel,forwardRefInfo,
     writeNewTemp,writeNewLabel,writeCode,
     writeRefCountType,writeOffsetType,writeRTTOffsetType,
     writeTemp,writeLabel,writeLabelRef,writeName,writeBranch,
     gen)
where

import Control.Applicative(Applicative(..))
import Control.Monad.State(State,execState,get,put)
import Data.Char(isAlphaNum,isAscii,ord)
import Data.Map(Map)
import qualified Data.Map as M

class Gen g where
    genCode :: String -> g ()
    genRefCountType :: g String
    genOffsetType :: g String
    genRTTOffsetType :: g String

data LLVMGenState fwd =
    LLVMGenState (Temp,Label,ForwardRef) (String,String,String)
                 (Map ForwardRef [(Temp,Label,Maybe fwd)])
                 [Map ForwardRef [(Temp,Label,Maybe fwd)] -> String]
newtype LLVMGen fwd a = LLVMGen (State (LLVMGenState fwd) a)
newtype Label = Label Int
newtype Temp = Temp Int
  deriving Eq
newtype ForwardRef = ForwardRef Int
  deriving (Eq,Ord)

data ForwardRefState = ForwardRefState (String,String,String) [String]
newtype ForwardRefGen a = ForwardRefGen (State ForwardRefState a)

instance Monad (LLVMGen fwd) where
    LLVMGen s >>= f = LLVMGen (s >>= (\ a -> let LLVMGen st = f a in st))
    return a = LLVMGen (return a)

instance Functor (LLVMGen fwd) where
    fmap f (LLVMGen s) = LLVMGen (fmap f s)

instance Applicative (LLVMGen fwd) where
    pure = return
    f <*> a = f >>= ($ a) . fmap
    a *> b = a >> b
    a <* b = a >>= (b >>) . return

instance Gen (LLVMGen fwd) where
    genCode newcode = do
        LLVMGenState counters auxTypes forwardRefs code <- getCodeGen
        putCodeGen
            (LLVMGenState counters auxTypes forwardRefs (const newcode:code))
    genRefCountType = do
        LLVMGenState _ (refCountType,_,_) _ _ <- getCodeGen
        return refCountType
    genOffsetType = do
        LLVMGenState _ (_,offsetType,_) _ _ <- getCodeGen
        return offsetType
    genRTTOffsetType = do
        LLVMGenState _ (_,_,rttOffsetType) _ _ <- getCodeGen
        return rttOffsetType

instance Monad ForwardRefGen where
    ForwardRefGen s >>= f =
        ForwardRefGen (s >>= (\ a -> let ForwardRefGen st = f a in st))
    return a = ForwardRefGen (return a)

instance Functor ForwardRefGen where
    fmap f (ForwardRefGen s) = ForwardRefGen (fmap f s)

instance Applicative ForwardRefGen where
    pure = return
    f <*> a = f >>= ($ a) . fmap
    a *> b = a >> b
    a <* b = a >>= (b >>) . return

instance Gen ForwardRefGen where
    genCode newcode = do
        ForwardRefState auxTypes code <- getForwardRefGen
        putForwardRefGen (ForwardRefState auxTypes (newcode:code))
    genRefCountType = do
        ForwardRefState (refCountType,_,_) _ <- getForwardRefGen
        return refCountType
    genOffsetType = do
        ForwardRefState (_,offsetType,_) _ <- getForwardRefGen
        return offsetType
    genRTTOffsetType = do
        ForwardRefState (_,_,rttOffsetType) _ <- getForwardRefGen
        return rttOffsetType

gen :: Int -> Int -> Int -> LLVMGen fwd () -> String
gen maxRefCount maxOffset maxRTTOffset (LLVMGen codeGen) =
    concatMap ($ refData) (reverse code)
  where
    LLVMGenState _ _ refData code =
        execState codeGen (LLVMGenState (Temp 0,Label 0,ForwardRef 0)
                                        auxTypes M.empty [])
    auxTypes =
        (getAuxType maxRefCount,getAuxType maxOffset,getAuxType maxRTTOffset)
    getAuxType maxVal | maxVal < 256 = "i8"
                      | maxVal < 65536 = "i16"
                      | otherwise = "i32"

forwardGen :: (String,String,String)
           -> ([(Temp,Label,Maybe fwd)] -> ForwardRefGen ())
           -> ForwardRef
           -> Map ForwardRef [(Temp,Label,Maybe fwd)]
           -> String
forwardGen auxTypes forwardRefGen forwardRef refData =
    concat (reverse code)
  where
    ForwardRefGen fwGen = forwardRefGen (refData M.! forwardRef)
    ForwardRefState _ code = execState fwGen (ForwardRefState auxTypes [])

newTemp :: LLVMGen fwd Temp
newTemp = do
    LLVMGenState (Temp t,label,forwardRef) auxTypes refData code <- getCodeGen
    putCodeGen
        (LLVMGenState (Temp (t+1),label,forwardRef) auxTypes refData code)
    return (Temp t)

newLabel :: LLVMGen fwd Label
newLabel = do
    LLVMGenState (temp,Label l,forwardRef) auxTypes refData code <- getCodeGen
    putCodeGen
        (LLVMGenState (temp,Label (l+1),forwardRef) auxTypes refData code)
    return (Label l)

writeNewTemp :: String -> LLVMGen fwd Temp
writeNewTemp code = do
    temp <- newTemp
    writeCode " "
    writeTemp temp (" = " ++ code)
    return temp

writeNewLabel :: LLVMGen fwd Label
writeNewLabel = do
    label <- newLabel
    writeLabel label
    return label

forwardRef :: ([(Temp,Label,Maybe fwd)] -> ForwardRefGen ())
              -> LLVMGen fwd ((Temp,Label,Maybe fwd) -> LLVMGen fwd ())
forwardRef fwGen = do
    LLVMGenState (temp,label,ref@(ForwardRef f)) auxTypes refData code <-
        getCodeGen
    putCodeGen (LLVMGenState (temp,label,ForwardRef (f+1)) auxTypes
                             (M.insert ref [] refData)
                             (newcode auxTypes ref:code))
    return (saveRefData ref)
  where
    newcode auxTypes ref = forwardGen auxTypes fwGen ref
    saveRefData forwardRef newData = do
        LLVMGenState counters auxTypes refData code <- getCodeGen
        putCodeGen (LLVMGenState counters auxTypes
                                 (M.update (Just . (newData:)) forwardRef
                                           refData)
                                 code)

forwardRefTemp :: (Temp -> ForwardRefGen ())
               -> LLVMGen fwd (Temp -> LLVMGen fwd ())
forwardRefTemp fwGen = do
    saveRefData <- forwardRef (\ [(temp,_,_)] -> fwGen temp)
    return (\ temp -> saveRefData (temp,error "forwardRefTemp",Nothing))

forwardRefLabel :: (Label -> ForwardRefGen ())
                -> LLVMGen fwd (Label -> LLVMGen fwd ())
forwardRefLabel fwGen = do
    saveRefData <- forwardRef (\ [(_,label,_)] -> fwGen label)
    return (\ label -> saveRefData (error "forwardRefLabel",label,Nothing))

forwardRefInfo :: (fwd -> ForwardRefGen ())
               -> LLVMGen fwd (fwd -> LLVMGen fwd ())
forwardRefInfo fwGen = do
    saveRefData <- forwardRef (\ [(_,_,Just info)] -> fwGen info)
    return (\ info ->
        saveRefData (error "forwardRefInfo",error "forwardRefInfo",Just info))

getCodeGen :: LLVMGen fwd (LLVMGenState fwd)
getCodeGen = LLVMGen get

putCodeGen :: LLVMGenState fwd -> LLVMGen fwd ()
putCodeGen = LLVMGen . put

getForwardRefGen :: ForwardRefGen ForwardRefState
getForwardRefGen = ForwardRefGen get

putForwardRefGen :: ForwardRefState -> ForwardRefGen ()
putForwardRefGen = ForwardRefGen . put

writeCode :: Gen g => String -> g ()
writeCode = genCode

writeRefCountType :: (Gen g, Monad g) => String -> g ()
writeRefCountType code = genRefCountType >>= genCode >> genCode code

writeOffsetType :: (Gen g, Monad g) => String -> g ()
writeOffsetType code = genOffsetType >>= genCode >> genCode code

writeRTTOffsetType :: (Gen g, Monad g) => String -> g ()
writeRTTOffsetType code = genRTTOffsetType >>= genCode >> genCode code

writeTemp :: Gen g => Temp -> String -> g ()
writeTemp (Temp t) code  = writeCode ("%" ++ show t ++ code)

writeLabel :: Gen g => Label -> g ()
writeLabel (Label l) = writeCode (" l" ++ show l ++ ":")

writeLabelRef :: Gen g => Label -> g ()
writeLabelRef (Label l) = writeCode ("%l" ++ show l)

writeName :: Gen g => String -> g ()
writeName "main" = writeCode "main"
writeName name = writeCode ("_" ++ concatMap escape name)
  where
    escape ch | isAscii ch && isAlphaNum ch = [ch]
              | otherwise = "_" ++ show (ord ch) ++ "_"

writeBranch :: Temp
            -> LLVMGen fwd (Label -> LLVMGen fwd (),Label -> LLVMGen fwd ())
writeBranch temp = do
    writeCode " br i1 "
    writeTemp temp ",label "
    trueLabelRef <- forwardRefLabel writeLabelRef
    writeCode ",label "
    falseLabelRef <- forwardRefLabel writeLabelRef
    return (trueLabelRef,falseLabelRef)
