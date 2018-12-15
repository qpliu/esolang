-- https://esolangs.org/wiki/Aubergine

import Data.Array(Array,array,bounds,inRange,(!),(//))
import Data.Char(chr,ord)

data State = State {
    a :: Int,
    b :: Int,
    i :: Int,
    cells :: Array Int Int
    }

parse :: String -> State
parse prog = State{a = 0, b = 0, i = 0, cells = array (0,length prog - 1) (zip [0..] (map ord prog))}

interp :: State -> String -> String
interp s@State{a = a, b = b, i = i, cells = cells} inp =
    step (cells!i) (cells!(i+1)) (cells!(i+2))
  where
    step 61 lhs rhs = let (r,inp') = rval rhs inp
                          (o,s') = lval lhs r
                      in  o ++ advance s' inp'
    step 43 lhs rhs = let (l,inp') = rval lhs inp
                          (r,inp'') = rval rhs inp'
                          (o,s') = lval lhs (l + r)
                      in  o ++ advance s' inp''    
    step 45 lhs rhs = let (l,inp') = rval lhs inp
                          (r,inp'') = rval rhs inp'
                          (o,s') = lval lhs (l - r)
                      in  o ++ advance s' inp''    
    step 58 lhs rhs = let (l,inp') = rval lhs inp
                          (r,inp'') = rval rhs inp'
                      in  advance s{i = if r == 0 then i else l} inp''
    step insn _ _ = error ("at " ++ show i ++ ": " ++ show insn)
    rval 49 inp = (1,inp)
    rval 65 inp = (cells!a,inp)
    rval 66 inp = (cells!b,inp)
    rval 97 inp = (a,inp)
    rval 98 inp = (b,inp)
    rval 105 inp = (i,inp)
    rval 111 [] = error "EOF"
    rval 111 inp = (ord (head inp),tail inp)
    rval insn _ = error ("at " ++ show i ++ ": " ++ show insn)
    lval 65 val = ("",s{cells = cells // [(a,val)]})
    lval 66 val = ("",s{cells = cells // [(b,val)]})
    lval 97 val = ("",s{a = val})
    lval 98 val = ("",s{b = val})
    lval 105 val = ("",s{i = val})
    lval 111 val = ([chr val],s)
    lval insn _ = error ("at " ++ show i ++ ": " ++ show insn)
    advance state@State{i = i} inp
      | not (inRange (bounds cells) i) = ""
      | otherwise = interp state{i = i + 3} inp

aubergine :: String -> IO ()
aubergine prog = interact (interp (parse prog))
