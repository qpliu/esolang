-- https://esolangs.org/wiki/Hollow

-- Build: ghc --make hollow
-- Usage: hollow SRC-FILE

import Data.Array(Array,array,bounds,inRange,(!))
import Data.Char(isSpace)
import System.Environment(getArgs)

data Line = Line {
    dataPart :: [DataPart],
    instruction :: Instruction,
    success :: Maybe Int,
    failure :: Maybe Int
    } deriving Show

data DataPart = C Char | Input | Pop deriving Show

data Instruction = Push Int | Search | Split | Output deriving Show

parseLine :: String -> Line
parseLine str
  | take 1 str == "{" = parseDataPart [] (drop 1 str)
  | otherwise = error "Syntax error"
  where
    parseDataPart dataPart "" = error "Syntax error"
    parseDataPart dataPart ('}':str) = parseInstruction (reverse dataPart) str
    parseDataPart dataPart ('.':'0':str) = parseDataPart (Input:dataPart) str
    parseDataPart dataPart ('.':'1':str) = parseDataPart (Pop:dataPart) str
    parseDataPart dataPart ('.':'6':str) = parseDataPart (C '.':dataPart) str
    parseDataPart dataPart ('.':'7':str) = parseDataPart (C '{':dataPart) str
    parseDataPart dataPart ('.':'8':str) = parseDataPart (C '}':dataPart) str
    parseDataPart dataPart ('.':'9':str) = parseDataPart (C '\n':dataPart) str
    parseDataPart dataPart ('.':str) = error "Syntax error"
    parseDataPart dataPart (c:str) = parseDataPart (C c:dataPart) str
    parseInstruction dataPart ('.':str) = parseSuccess dataPart (Push 0) str
    parseInstruction dataPart ('+':str) = parseSuccess dataPart (Push 2) str
    parseInstruction dataPart ('\\':str) = parseSuccess dataPart Search str
    parseInstruction dataPart ('=':str) = parseSuccess dataPart Split str
    parseInstruction dataPart (':':str) = parseSuccess dataPart Output str
    parseInstruction dataPart str = parseSuccess dataPart (Push 1) str
    parseSuccess dataPart instruction "" = parseDone dataPart instruction (Just 0) (Just 0) ""
    parseSuccess dataPart instruction ('?':str) = parseFailure dataPart instruction Nothing str
    parseSuccess dataPart instruction str = parseFailure dataPart instruction (Just (length (takeWhile (== '>') str) - length (takeWhile (=='<') str))) (dropWhile (== '>') (dropWhile (== '<') str))
    parseFailure dataPart instruction success ('|':'?':str) = parseDone dataPart instruction success Nothing (dropWhile isSpace str)
    parseFailure dataPart instruction success ('|':str) = parseDone dataPart instruction success (Just (length (takeWhile (== '>') str) - length (takeWhile (=='<') str))) (dropWhile isSpace (dropWhile (== '>') (dropWhile (== '<') str)))
    parseFailure dataPart instruction success str = parseDone dataPart instruction success (Just 0) (dropWhile isSpace str)
    parseDone dataPart instruction success failure "" = Line{dataPart=dataPart,instruction=instruction,success=success,failure=failure}
    parseDone dataPart instruction success failure str = error "Syntax error"

parse :: String -> Array Int Line
parse prog = array (1,length ls) (zip [1..] ls)
  where
    ls = map parseLine (filter (not . null) (map (dropWhile isSpace) (lines prog)))

interp :: Array Int Line -> Int -> [String] -> String -> String
interp lines lineNo stack inp
  | not (inRange (bounds lines) lineNo) = interp lines lineNo stack inp
  | otherwise = interpLine (lines!lineNo)
  where
    interpLine Line{dataPart=dataPart,instruction=instruction,success=success,failure=failure} =
        interpInstruction (interpDataPart stack inp success failure "" dataPart) failure instruction
    interpDataPart stack inp success failure str [] = (stack,inp,success,reverse str)
    interpDataPart stack inp success failure str (C ch:dataPart) = interpDataPart stack inp success failure (ch:str) dataPart
    interpDataPart stack inp success failure str (Input:dataPart)
      | null inp = interpDataPart stack inp failure failure str dataPart
      | otherwise = interpDataPart stack (tail inp) success failure (head inp:str) dataPart
    interpDataPart stack inp success failure str (Pop:dataPart)
      | null stack = interpDataPart stack inp failure failure str dataPart
      | otherwise = interpDataPart (tail stack) inp success failure (reverse (head stack) ++ str) dataPart
    interpInstruction (stack,inp,success,dataPart) failure (Push n) =
        controlFlow inp (success,replicate n dataPart ++ stack)
    interpInstruction (stack,inp,success,dataPart) failure Search =
        maybe (controlFlow inp (failure,stack))
                (controlFlow inp . (,) success)
                (search dataPart [] (reverse stack))
    interpInstruction (stack,inp,success,dataPart) failure Split
      | null stack || null dataPart = controlFlow inp (failure,stack)
      | otherwise = maybe (controlFlow inp (failure,stack))
                (controlFlow inp . (,) success . (++ tail stack))
                (split dataPart (head stack))
    interpInstruction (stack,inp,success,dataPart) failure Output =
        dataPart ++ controlFlow inp (success,stack)
    controlFlow inp (Nothing,stack) = ""
    controlFlow inp (Just n,stack) = interp lines (lineNo + n) stack inp

split :: String -> String -> Maybe [String]
split sep str = s "" str
  where
    s prefix str
      | length str < length sep = Nothing
      | sep == take (length sep) str = Just [reverse prefix,drop (length sep) str]
      | otherwise = s (head str:prefix) (tail str)


search :: String -> [String] -> [String] -> Maybe [String]
search target bottom top
  | null top = Nothing
  | otherwise = maybe (search target (head top:bottom) (tail top))
                (const (Just (head top:reverse (tail top) ++ bottom)))
                (split target (head top))

run :: String -> String -> String
run prog = interp (parse prog) 1 []

hollow :: String -> IO ()
hollow prog = interact (run prog)

main :: IO ()
main = getArgs >>= readFile . head >>= hollow
