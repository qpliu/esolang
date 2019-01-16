-- https://esolangs.org/wiki////

-- Build: ghc --make subst
-- Usage: subst SRC-FILE

import System.Environment(getArgs)

execute :: String -> String
execute [] = []
execute ('\\':ch:rest) = ch : execute rest
execute ('/':rest) = pattern [] rest
execute (ch:rest) = ch : execute rest

pattern :: String -> String -> String
pattern pat [] = []
pattern pat ('\\':ch:rest) = pattern (ch:pat) rest
pattern pat ('/':rest) = replacement (reverse pat) [] rest
pattern pat (ch:rest) = pattern (ch:pat) rest

replacement :: String -> String -> String -> String
replacement pat rep [] = []
replacement pat rep ('\\':ch:rest) = replacement pat (ch:rep) rest
replacement pat rep ('/':rest) = substitution pat (reverse rep) [] rest
replacement pat rep (ch:rest) = replacement pat (ch:rep) rest

substitution :: String -> String -> String -> String -> String
substitution pat rep leader rest
  | null rest = execute (reverse leader)
  | pat == take (length pat) rest =
      substitution pat rep [] (reverse leader ++ rep ++ drop (length pat) rest)
  | otherwise = substitution pat rep (head rest:leader) (tail rest)

subst :: String -> IO ()
subst = putStr . execute

main :: IO ()
main = getArgs >>= readFile . head >>= subst
