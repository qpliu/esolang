-- https://esolangs.org/wiki/Unlambda

-- Build: ghc --make unlambda
-- Usage: unlambda SRC-FILE

import System.Environment(getArgs)

data F = Ap F F | S | K | I | V | Ch Char | D | C | E | In | Test Char | Print
       | ApK F | ApS F | ApApS F F | ApD F
       | ApC (String -> Maybe Char -> F -> String)

parse :: String -> F
parse prog = fst (p prog)
  where
    p src
      | null src = error "Unexpected EOF"
      | c == '#' = p (dropWhile (/= '\n') src)
      | c == ' ' || c == '\r' || c == '\t' || c == '\n' = p (tail src)
      | c == '`' =
            let (l,src2) = p (tail src)
                (r,src3) = p src2
            in  (Ap l r,src3)
      | c == 's' = (S,tail src)
      | c == 'k' = (K,tail src)
      | c == 'i' = (I,tail src)
      | c == 'v' = (V,tail src)
      | c == '.' = (Ch (head (tail src)),tail (tail src))
      | c == 'r' = (Ch '\n',tail src)
      | c == 'd' = (D,tail src)
      | c == 'c' = (C,tail src)
      | c == 'e' = (E,tail src)
      | c == '@' = (In,tail src)
      | c == '?' = (Test (head (tail src)),tail (tail src))
      | c == '|' = (Print,tail src)
      | otherwise = error ("Unknown char: " ++ [c])
      where
        c = head src

eval :: String -> Maybe Char -> F -> (String -> Maybe Char -> F -> String) -> String
eval inp ch (Ap D g) cont = cont inp ch (ApD g)
eval inp ch (Ap f g) cont = eval inp ch f eval2
  where
    eval2 inp ch ef = eval inp ch g (evalAp ef)
    evalAp ef inp ch eg = apply inp ch ef eg cont
eval inp ch f cont = cont inp ch f

apply :: String -> Maybe Char -> F -> F -> (String -> Maybe Char -> F -> String) -> String
apply inp ch S f cont = cont inp ch (ApS f)
apply inp ch K f cont = cont inp ch (ApK f)
apply inp ch I f cont = cont inp ch f
apply inp ch V f cont = cont inp ch V
apply inp ch (Ch char) f cont = char : cont inp ch f
apply inp ch D f cont = error "`d special form"
apply inp ch C f cont = apply inp ch f (ApC cont) cont
apply inp ch E f cont = ""
apply "" ch In f cont = apply "" Nothing f V cont
apply (c:cs) ch In f cont = apply cs (Just c) f I cont
apply inp Nothing (Test _) f cont = apply inp Nothing f V cont
apply inp ch@(Just c) (Test t) f cont
  | c == t = apply inp ch f I cont
  | otherwise = apply inp ch f V cont
apply inp Nothing Print f cont = apply inp Nothing f V cont
apply inp ch@(Just c) Print f cont = c : apply inp ch f I cont
apply inp ch (ApK f) g cont = cont inp ch f
apply inp ch (ApS f) g cont = cont inp ch (ApApS f g)
apply inp ch (ApApS f g) h cont = apply inp ch f h s2
  where
    s2 inp ch fh = apply inp ch g h (s3 fh)
    s3 fh inp ch gh = apply inp ch fh gh cont
apply inp ch (ApD f) g cont = eval inp ch f d2
  where
    d2 inp ch df = apply inp ch df g cont
apply inp ch (ApC cont) f _ = cont inp ch f

run :: String -> String -> String
run prog inp = eval inp Nothing (parse prog) (const (const (const "")))

unlambda :: String -> IO ()
unlambda prog = interact (run prog)

main :: IO ()
main = getArgs >>= readFile . head >>= unlambda
