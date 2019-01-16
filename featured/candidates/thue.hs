-- https://esolangs.org/wiki/Thue

-- Build: ghc --make thue
-- Usage: thue SRC-FILE

import System.Environment(getArgs)

data Rule = S String String | I String | O String String

search :: String -> String -> Maybe (String,String)
search substr str = s 0
  where
    s n | n > length str - length substr = Nothing
        | substr == take (length substr) (drop n str) =
                Just (take n str,drop (n + length substr) str)
        | otherwise = s (n+1)

parse :: String -> ([Rule],String)
parse prog = p (lines prog) []
  where
    p [] _ = error "No termination"
    p ("":strs) rules = p strs rules
    p (str:strs) rules = parseRule strs rules (search "::=" str)
    parseRule strs rules Nothing = error "Invalid rule"
    parseRule strs rules (Just ("","")) = (rules,unlines strs)
    parseRule strs rules (Just (substr,":::")) = p strs (I substr:rules)
    parseRule strs rules (Just (substr,'~':output)) = p strs (O substr output:rules)
    parseRule strs rules (Just (substr,replace)) = p strs (S substr replace:rules)

interp :: [String] -> [Rule] -> [Rule] -> String -> String
interp inp [] unmatching state = ""
interp inp (rule@(S substr replace):rules) unmatching state =
    maybe (interp inp rules (rule:unmatching) state) s (search substr state)
  where
    s (before,after) = interp inp (rules ++ rule:unmatching) [] (before ++ replace ++ after)
interp inp (rule@(I substr):rules) unmatching state =
    maybe (interp inp rules (rule:unmatching) state) i (search substr state)
  where
    i (before,after) | null inp = interp inp (rules ++ rule:unmatching) [] (before ++ after)
                     | otherwise = interp (tail inp) (rules ++ rule:unmatching) [] (before ++ head inp ++ after)
interp inp (rule@(O substr output):rules) unmatching state =
    maybe (interp inp rules (rule:unmatching) state) o (search substr state)
  where
    o (before,after) = modify output ++ interp inp (rules ++ rule:unmatching) [] (before ++ after)
    modify output | output == "" = "\n" | otherwise = output

run :: ([Rule],String) -> String -> String
run (rules,state) inp = interp (lines inp) rules [] state

thue :: String -> IO ()
thue prog = interact (run (parse prog))

main :: IO ()
main = getArgs >>= readFile . head >>= thue
