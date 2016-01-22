module LLVMGen
    (CodeGen,Label,Temp,
     newTemp,newLabel,forwardRef,forwardRefTemp,forwardRefLabel,
     writeNewTemp,writeNewLabel,writeCode,writeRefCountType,writeOffsetType,
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

data CodeGenState = CodeGenState (Temp,Label,ForwardRef) (String,String)
                                 (Map ForwardRef [(Temp,Label)])
                                 [Map ForwardRef [(Temp,Label)] -> String]
newtype CodeGen a = CodeGen (State CodeGenState a)
newtype Label = Label Int
newtype Temp = Temp Int
  deriving Eq
newtype ForwardRef = ForwardRef Int
  deriving (Eq,Ord)

data ForwardRefState = ForwardRefState (String,String) [String]
newtype ForwardRefGen a = ForwardRefGen (State ForwardRefState a)

instance Monad CodeGen where
    CodeGen s >>= f = CodeGen (s >>= (\ a -> let CodeGen st = f a in st))
    return a = CodeGen (return a)

instance Functor CodeGen where
    fmap f (CodeGen s) = CodeGen (fmap f s)

instance Applicative CodeGen where
    pure = return
    f <*> a = f >>= ($ a) . fmap
    a *> b = a >> b
    a <* b = a >>= (b >>) . return

instance Gen CodeGen where
    genCode newcode = do
        CodeGenState counters auxTypes forwardRefs code <- getCodeGen
        putCodeGen
            (CodeGenState counters auxTypes forwardRefs (const newcode:code))
    genRefCountType = do
        CodeGenState _ (refCountType,_) _ _ <- getCodeGen
        return refCountType
    genOffsetType = do
        CodeGenState _ (_,offsetType) _ _ <- getCodeGen
        return offsetType

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
        ForwardRefState (refCountType,_) _ <- getForwardRefGen
        return refCountType
    genOffsetType = do
        ForwardRefState (_,offsetType) _ <- getForwardRefGen
        return offsetType

gen :: Int -> Int -> CodeGen () -> String
gen maxRefCount maxOffset (CodeGen codeGen) =
    concatMap ($ refData) (reverse code)
  where
    CodeGenState _ _ refData code =
        execState codeGen (CodeGenState (Temp 0,Label 0,ForwardRef 0)
                                        auxTypes M.empty [])
    auxTypes = (getAuxType maxRefCount,getAuxType maxOffset)
    getAuxType maxVal | maxVal < 256 = "i8"
                      | maxVal < 65536 = "i16"
                      | otherwise = "i32"

forwardGen :: (String,String) -> ([(Temp,Label)] -> ForwardRefGen ())
                              -> ForwardRef
                              -> Map ForwardRef [(Temp,Label)]
                              -> String
forwardGen auxTypes forwardRefGen forwardRef refData =
    concat (reverse code)
  where
    ForwardRefGen fwGen = forwardRefGen (refData M.! forwardRef)
    ForwardRefState _ code = execState fwGen (ForwardRefState auxTypes [])

newTemp :: CodeGen Temp
newTemp = do
    CodeGenState (Temp t,label,forwardRef) auxTypes refData code <- getCodeGen
    putCodeGen
        (CodeGenState (Temp (t+1),label,forwardRef) auxTypes refData code)
    return (Temp t)

newLabel :: CodeGen Label
newLabel = do
    CodeGenState (temp,Label l,forwardRef) auxTypes refData code <- getCodeGen
    putCodeGen
        (CodeGenState (temp,Label (l+1),forwardRef) auxTypes refData code)
    return (Label l)

writeNewTemp :: CodeGen Temp
writeNewTemp = do
    temp <- newTemp
    writeCode " "
    writeTemp temp
    writeCode " = "
    return temp

writeNewLabel :: CodeGen Label
writeNewLabel = do
    label <- newLabel
    writeLabel label
    return label

forwardRef :: ([(Temp,Label)] -> ForwardRefGen ())
              -> CodeGen ((Temp,Label) -> CodeGen ())
forwardRef fwGen = do
    CodeGenState (temp,label,ref@(ForwardRef f)) auxTypes refData code <-
        getCodeGen
    putCodeGen (CodeGenState (temp,label,ForwardRef (f+1)) auxTypes
                             (M.insert ref [] refData)
                             (newcode auxTypes ref:code))
    return (saveRefData ref)
  where
    newcode auxTypes ref = forwardGen auxTypes fwGen ref
    saveRefData forwardRef newData = do
        CodeGenState counters auxTypes refData code <- getCodeGen
        putCodeGen (CodeGenState counters auxTypes
                                 (M.update (Just . (newData:)) forwardRef
                                           refData)
                                 code)

forwardRefTemp :: (Temp -> ForwardRefGen ()) -> CodeGen (Temp -> CodeGen ())
forwardRefTemp fwGen = do
    saveRefData <- forwardRef (\ [(temp,_)] -> fwGen temp)
    return (\ temp -> saveRefData (temp,error "forwardRefTemp"))

forwardRefLabel :: (Label -> ForwardRefGen ()) -> CodeGen (Label -> CodeGen ())
forwardRefLabel fwGen = do
    saveRefData <- forwardRef (\ [(_,label)] -> fwGen label)
    return (\ label -> saveRefData (error "forwardRefLabel",label))

getCodeGen :: CodeGen CodeGenState
getCodeGen = CodeGen get

putCodeGen :: CodeGenState -> CodeGen ()
putCodeGen = CodeGen . put

getForwardRefGen :: ForwardRefGen ForwardRefState
getForwardRefGen = ForwardRefGen get

putForwardRefGen :: ForwardRefState -> ForwardRefGen ()
putForwardRefGen = ForwardRefGen . put

writeCode :: Gen g => String -> g ()
writeCode = genCode

writeRefCountType :: (Gen g, Monad g) => g ()
writeRefCountType = genRefCountType >>= genCode

writeOffsetType :: (Gen g, Monad g) => g ()
writeOffsetType = genOffsetType >>= genCode

writeTemp :: Gen g => Temp -> g ()
writeTemp (Temp t) = writeCode ("%" ++ show t)

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

writeBranch :: Temp -> CodeGen (Label -> CodeGen (),Label -> CodeGen ())
writeBranch temp = do
    writeCode " br i1 "
    writeTemp temp
    writeCode ", label "
    trueLabelRef <- forwardRefLabel writeLabelRef
    writeCode ", label "
    falseLabelRef <- forwardRefLabel writeLabelRef
    return (trueLabelRef,falseLabelRef)
