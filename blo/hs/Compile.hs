module Compile
    (Compile(..),CompileError(..),SourcePos(..),compileError)
where

import Control.Applicative(Applicative(..))
import Text.ParserCombinators.Parsec(SourcePos(..))

newtype Compile a = Compile (Either CompileError a)
data CompileError = CompileError SourcePos String
  deriving Show

instance Monad Compile where
    (Compile a) >>= f = either (Compile . Left) f a
    return = Compile . Right
    fail = error

instance Functor Compile where
    fmap f (Compile a) = Compile (either Left (Right . f) a)

instance Applicative Compile where
    pure = return
    f <*> a = f >>= ($ a) . fmap
    a *> b = a >> b
    a <* b = a >>= (b >>) . return

compileError :: SourcePos -> String -> Compile a
compileError pos msg = (Compile . Left . CompileError pos) msg
