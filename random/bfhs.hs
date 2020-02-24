module Bfhs(Brainfuck,plus,minus,gt,lt,dot,comma,bracket,bf,bf8,parse)
where

-- An implementation of brainfuck as compositions of state transformers

import Data.Word(Word8)

data Brainfuck int a = Brainfuck (([int],[int],[int]) -> (a,[int],[int],[int],[int]))

instance Monad (Brainfuck int) where
  Brainfuck f >>= g = Brainfuck (\ (input,left,right) ->
    let (a',output',input',left',right') = f (input,left,right)
        Brainfuck h = g a'
        (a'',output'',input'',left'',right'') = h (input',left',right')
    in  (a'',output'++output'',input'',left'',right''))
  return a = Brainfuck (\ (input,left,right) -> (a,[],input,left,right))

instance Functor (Brainfuck int) where
  fmap f (Brainfuck g) = Brainfuck (\ (input,left,right) ->
    let (a',output',input',left',right') = g (input,left,right)
    in  (f a',output',input',left',right'))

instance Applicative (Brainfuck int) where
    pure = return
    f <*> a = f >>= ($ a) . fmap
    a *> b = a >> b
    a <* b = a >>= (b >>) . return

plus :: (Bounded int,Enum int,Eq int) => Brainfuck int ()
plus = Brainfuck (\ (input,cell:left,right) ->
  if cell == maxBound
    then ((),[],input,minBound:left,right)
    else ((),[],input,succ cell:left,right))

minus :: (Bounded int,Enum int,Eq int) => Brainfuck int ()
minus = Brainfuck (\ (input,cell:left,right) ->
  if cell == minBound
    then ((),[],input,maxBound:left,right)
    else ((),[],input,pred cell:left,right))

lt :: Brainfuck int ()
lt = Brainfuck (\ (input,cell:left,right) -> ((),[],input,left,cell:right))

gt :: Brainfuck int ()
gt = Brainfuck (\ (input,left,cell:right) -> ((),[],input,cell:left,right))

dot :: Brainfuck int ()
dot = Brainfuck (\ (input,left@(cell:_),right) -> ((),[cell],input,left,right))

comma :: Brainfuck int ()
comma = Brainfuck (\ (input,cell:left,right) ->
  if null input
    then ((),[],input,cell:left,right)
    else ((),[],tail input,head input:left,right))

bracket :: (Bounded int,Eq int) => [Brainfuck int ()] -> Brainfuck int ()
bracket body = Brainfuck (\ (input,left@(cell:_),right) ->
  if cell == minBound
    then ((),[],input,left,right)
    else runbf (sequence_ (body ++ [bracket body])) (input,left,right))

runbf :: Brainfuck int a -> ([int],[int],[int]) -> (a,[int],[int],[int],[int])
runbf (Brainfuck f) (input,left,right) = f (input,left,right)

bf :: Bounded int => Brainfuck int () -> [int] -> [int]
bf code input =
  let (_,output,_,_,_) = runbf code (input,repeat minBound,repeat minBound)
  in  output

bf8 :: Brainfuck Word8 () -> String -> String
bf8 code input = map (toEnum . fromIntegral) (bf code (map (fromIntegral . fromEnum) input))

parse :: (Bounded int,Enum int,Eq int) => String -> Brainfuck int ()
parse code = let (_,ops) = parseOps (code,[]) in sequence_ ops
  where
    parseOps ("",ops) = ("",reverse ops)
    parseOps ('+':code,ops) = parseOps (code,plus:ops)
    parseOps ('-':code,ops) = parseOps (code,minus:ops)
    parseOps ('<':code,ops) = parseOps (code,lt:ops)
    parseOps ('>':code,ops) = parseOps (code,gt:ops)
    parseOps ('.':code,ops) = parseOps (code,dot:ops)
    parseOps (',':code,ops) = parseOps (code,comma:ops)
    parseOps ('[':code,ops) = let (code',ops') = parseOps (code,[])
                              in parseOps (code',bracket ops':ops)
    parseOps (']':code,ops) = (code,reverse ops)
    parseOps (_:code,ops) = parseOps (code,ops)
