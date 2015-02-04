-- http://esolangs.org/wiki/Deadfish

df :: Integer -> String -> String
df x s
 | null s = cycle "\n>> "
 | x == -1 || x == 256 = df 0 s
 | head s == 'i' = ">> " ++ df (x+1) (tail s)
 | head s == 'd' = ">> " ++ df (x-1) (tail s)
 | head s == 's' = ">> " ++ df (x*x) (tail s)
 | head s == 'o' = (show x) ++ "\n>> " ++ df x (tail s)
 | otherwise = "\n>> " ++ df x (tail s)

deadfish :: IO ()
deadfish = putStr ">> " >> interact (df 0)
