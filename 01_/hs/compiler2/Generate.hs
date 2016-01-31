{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses #-}
module Generate
    (Gen(..),Generate,ForwardGen,forwardRef,putState,generate)
where

import Control.Applicative(Applicative(..))
import Control.Monad.State(State,execState,get,put)
import Data.Map(Map)
import qualified Data.Map as Map

class Gen gen state result where
    gen :: result -> gen state result ()
    getState :: gen state result state

newtype Generate forward state result a =
    Generate (State (GenerateState forward state result) a)
data GenerateState forward state result =
    GenerateState ForwardRef
                  state
                  (Map ForwardRef [forward])
                  [Map ForwardRef [forward] -> [result]]

newtype ForwardRef = ForwardRef Int deriving (Eq,Ord)

newtype ForwardGen state result a =
    ForwardGen (State (ForwardGenState state result) a)
data ForwardGenState state result = ForwardGenState state [result]

instance Monad (Generate forward state result) where
    Generate s >>= f = Generate (s >>= (\ a -> let Generate st = f a in st))
    return a = Generate (return a)

instance Functor (Generate forward state result) where
    fmap f (Generate s) = Generate (fmap f s)

instance Applicative (Generate forward state result) where
    pure = return
    f <*> a = f >>= ($ a) . fmap
    a *> b = a >> b
    a <* b = a >>= (b >>) . return

instance Gen (Generate forward) state result where
    gen result = do
        (GenerateState forwardRef state forwards results) <- Generate get
        Generate (put (GenerateState forwardRef state forwards
                                     (results ++ [const [result]])))
    getState = do
        (GenerateState _ state _ _) <- Generate get
        return state

generate :: Generate forward state result () -> state -> [result]
generate (Generate gen) state =
    let GenerateState _ _ forwards results =
            execState gen (GenerateState (ForwardRef 0) state Map.empty [])
    in  concatMap ($ forwards) results

forwardRef :: ([forward] -> ForwardGen result state ())
                    ->  Generate forward result state
                                 (forward -> Generate forward result state ())
forwardRef forwardGen = do
    (GenerateState fwdId@(ForwardRef f) state forwards results) <- Generate get
    Generate (put (GenerateState (ForwardRef (f+1)) state forwards
                                             (results ++ [fwdGen fwdId])))
    return (ref fwdId)
  where
    fwdGen fwdId forwards = 
        let ForwardGen gen =
                forwardGen (maybe [] id (Map.lookup fwdId forwards))
            ForwardGenState state res =
                execState gen (ForwardGenState state [])
        in  reverse res
    ref fwdId fwd = do
        (GenerateState fwdRef state forwards results) <- Generate get
        Generate (put (GenerateState fwdRef state (update forwards) results))
      where
        update = Map.alter (maybe (Just [fwd]) (Just . (fwd:))) fwdId

putState :: state -> Generate forward state result ()
putState state = do
    (GenerateState forwardRef _ forwards results) <- Generate get
    Generate (put (GenerateState forwardRef state forwards results))

instance Monad (ForwardGen state result) where
    ForwardGen s >>= f =
        ForwardGen (s >>= (\ a -> let ForwardGen st = f a in st))
    return a = ForwardGen (return a)

instance Functor (ForwardGen state result) where
    fmap f (ForwardGen s) = ForwardGen (fmap f s)

instance Applicative (ForwardGen state result) where
    pure = return
    f <*> a = f >>= ($ a) . fmap
    a *> b = a >> b
    a <* b = a >>= (b >>) . return

instance Gen ForwardGen state result where
    gen result = do
        (ForwardGenState state results) <- ForwardGen get
        ForwardGen (put (ForwardGenState state (result:results)))
    getState = do
        (ForwardGenState state _) <- ForwardGen get
        return state
