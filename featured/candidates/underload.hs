-- https://esolangs.org/wiki/Underload

str :: String -> (String,String)
str "" = error "Unmatched ("
str ('(':src) =
    let (s,src2) = str src
        (s2,src3) = str src2
    in ("(" ++ s ++ ")" ++ s2,src3)
str (')':src) = ("",src)
str (c:src) = let (s,src2) = str src in (c:s,src2)

interp :: [String] -> String -> String
interp stack "" = ""
interp stack ('~':src) = interp (head (tail stack):head stack:drop 2 stack) src
interp stack (':':src) = interp (head stack:stack) src
interp stack ('!':src) = interp (tail stack) src
interp stack ('*':src) = interp ((head (tail stack) ++ head stack):drop 2 stack) src
interp stack ('(':src) = let (s,src2) = str src in interp (s:stack) src2
interp stack ('a':src) = interp (("(" ++ head stack ++ ")"):tail stack) src
interp stack ('^':src) = interp (tail stack) (head stack ++ src)
interp stack ('S':src) = head stack ++ interp (tail stack) src
interp stack (c:src) = error ("Invalid command: " ++ [c])

underload :: String -> IO ()
underload prog = putStr (interp [] prog)
