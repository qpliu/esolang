-- https://esolangs.org/wiki/Sortle

-- Build: ghc --make sortle
-- Usage: sortle SRC-FILE

-- Notes:

-- The pdf specification is at
-- https://github.com/graue/esofiles/blob/master/sortle/doc/sortle.pdf

-- The perl implementation is at
-- https://github.com/graue/esofiles/blob/master/sortle/impl/sortle.pl

-- It's not clear from the specification that the next expression to be
-- evaluated is the expression after where the last expression is inserted,
-- and not the expression after where the last expression was, which was
-- how I originally interpreted it to be.

-- The specification says that the initial names can only be alphabetical
-- characters.  The perl implementation allows alphanumeric characters in
-- the initial names, excluding the first character.  This implementation
-- follows the specification.

-- The pdf specification says that all numbers are modulo 4294967296.  The
-- perl uses the default int size and allows negative numbers and suggests
-- using bignums in the future.  This implementation follows the
-- specification.

-- The specification does not say what order regex matching of the names is
-- done.  A comment in the perl implementation says it is in reverse order,
-- starting from the current expression.  This implementation follows the
-- comment in the perl implementation.

-- The specification does not say how to do numerical operations on
-- non-numerical operands.  The perl implementation treats non-numerical
-- operands as zero.  This implementation follows the perl implementation,
-- but does not follow the perl implementation's treatment of leading + or -
-- as the sign, instead intepreting strings with leading + or - as
-- non-numerical, resulting in zero.

-- The pdf specification says matching "(f.n)!d" with "finfund" results in
-- "fin".  The perl implementation results in "finfun".  Some of the example
-- code do not work when following the pdf specification, so this
-- implementation follows the perl implementation in this case.  For regexes
-- with multiple captures, this implementation probably diverges from the
-- perl implementation.

-- The pdf specification disallows nested regex element groups.  This
-- specification allows nested regex element groups.  Nested captures will
-- probably result in unexpected results, in which the nested captures are
-- repeated.

import Data.Char(chr,isAlpha,isDigit,isSpace)
import Data.Word(Word32)
import System.Environment(getArgs)

data Expr = ExprLiteral String | ExprOperator ([String] -> String -> String -> String)

parse :: String -> [(String,(String,[Expr]))]
parse prog = foldl parseLine [] (lines prog)
  where
    parseLine exprs line
      | null name && take 1 beforeAssign `elem` ["","#"] = exprs
      | take 2 beforeAssign /= ":=" = error ("Invalid definition:"++line)
      | otherwise = insert (name,(line,parseExprs (dropWhile isSpace (drop 2 beforeAssign)))) exprs
      where
        (name,afterName) = span isAlpha (dropWhile isSpace line)
        beforeAssign = dropWhile isSpace afterName
        parseExprs str
          | null str || head str == '#' = []
          | head str == '"' =
                let (string,rest) = parseString (tail str) ""
                in  ExprLiteral string : parseExprs (dropWhile isSpace rest)
          | isDigit (head str) =
                let (num,rest) = span isDigit str
                in  ExprLiteral (toString (toNumber num)) : parseExprs (dropWhile isSpace rest)
          | otherwise = maybe (error ("Invalid expression:"++line)) ExprOperator (lookup (head str) operators) : parseExprs (dropWhile isSpace (tail str))
        parseString str acc
          | null str = error ("Invalid expression:"++line)
          | head str == '"' = (reverse acc,tail str)
          | head str == '\\' = parseString (drop 3 str) (parseEscape (take 2 (tail str)):acc)
          | otherwise = parseString (tail str) (head str:acc)
        parseEscape [c1,c2] = chr (16*maybe escapeError id (lookup c1 hexDigits)+maybe escapeError id (lookup c2 hexDigits))
        parseEscape _ = escapeError
        escapeError = error ("Invalid escape sequence:"++line)
        hexDigits = zip ['0'..'9'] [0..9] ++ zip ['a'..'f'] [10..15] ++ zip ['A'..'F'] [10..15]
    operators = [
        ('+',numOp (+)), ('*',numOp (*)), ('/',numOp div), ('%',numOp rem),
        ('^',const max), ('~',const (flip (++))), ('?',regex), ('$',const max)
        ]
      where
        numOp op = \ _ op1 op2 -> toString (op (toNumber op1) (toNumber op2))

insert :: (String,(String,[Expr])) -> [(String,(String,[Expr]))] -> [(String,(String,[Expr]))]
insert item@(name,_) [] = [item]
insert item@(name,_) list@(top@(topName,_):rest)
  | name == topName = item:rest
  | name < topName = item:list
  | otherwise = top:insert item rest

toNumber :: String -> Word32
toNumber s = fromIntegral (read ('0':takeWhile isDigit s) :: Integer)

toString :: Word32 -> String
toString 0 = ""
toString n = show n

data Regex = REGroup [Regex] | RECapture [Regex] | REChar Char | REAnyByte
           | REOneOrZero Regex | REOneOrMore Regex

parseRegex :: String -> [Regex]
parseRegex pattern = fst (parseGroup "" pattern [])
  where
    parseGroup end s re
      | take 1 s == end = (reverse re,drop 1 s)
      | null s || (null re && head s `elem` "!@") =
            error ("Invalid regex pattern:" ++ pattern)
      | head s == '(' =
            let (group,rest) = parseGroup ")" (tail s) []
            in  parseGroup end rest (RECapture group:re)
      | head s == '[' =
            let (group,rest) = parseGroup "]" (tail s) []
            in  parseGroup end rest (REGroup group:re)
      | head s == '.' = parseGroup end (tail s) (REAnyByte:re)
      | head s == '@' = parseGroup end (tail s) (REOneOrZero (head re):tail re)
      | head s == '!' = parseGroup end (tail s) (REOneOrMore (head re):tail re)
      | otherwise = parseGroup end (tail s) (REChar (head s):re)

matchRegex :: String -> [Regex] -> Maybe String
matchRegex s re = finalResult (matchGroup s re Nothing "")
  where
    finalResult (Just ("",(Just capture,_))) = Just (reverse capture)
    finalResult (Just ("",(Nothing,group)))= Just (reverse group)
    finalResult _ = Nothing
    matchGroup str [] capture matched = Just (str,(capture,matched))
    matchGroup ""  _  capture matched = Nothing
    matchGroup str (REGroup reGroup:res) capture matched =
        maybe Nothing continueMatch (matchGroup str reGroup Nothing "")
      where
        continueMatch (strAfterGroup,(groupCapture,groupMatched)) =
            matchGroup strAfterGroup res (maybe groupCapture Just capture) (groupMatched ++ matched)
    matchGroup str (RECapture reCapture:res) capture matched =
        maybe Nothing continueMatch (matchGroup str reCapture Nothing "")
      where
        continueMatch (strAfterGroup,(groupCapture,groupMatched)) =
            matchGroup strAfterGroup res (maybe (Just groupMatched) Just capture) (groupMatched ++ matched)
    matchGroup str (REChar ch:res) capture matched
      | head str == ch = matchGroup (tail str) res capture (ch:matched)
      | otherwise = Nothing
    matchGroup str (REAnyByte:res) capture matched =
        matchGroup (tail str) res capture (head str:matched)
    matchGroup str (REOneOrZero re:res) capture matched =
        maybe tryMatchOne Just (matchToEnd (matchGroup str res (Just (maybe "" id capture)) matched))
      where
        tryMatchOne = maybe Nothing continueMatch (matchGroup str [re] Nothing "")
        -- following perl implementation instead of specification, to follow the specification replace (Just . maybe id (++) groupCapture) with Just
        continueMatch (strAfterGroup,(groupCapture,groupMatched)) =
            matchGroup strAfterGroup res (maybe groupCapture (Just . maybe id (++) groupCapture) capture) (groupMatched ++ matched)
    matchGroup str (REOneOrMore re:res) capture matched = matchOneMore str capture matched
      where
        matchOneMore str capture matched = maybe Nothing (continueMatch capture matched) (matchGroup str [re] Nothing "")
        continueMatch capture matched (strAfterGroup,(groupCapture,groupMatched)) =
            maybe (matchOneMore strAfterGroup newCapture newMatched) Just (matchToEnd (matchGroup strAfterGroup res newCapture newMatched))
          where
            -- following perl implementation instead of specification, to follow the specification replace (Just . maybe id (++) groupCapture) with Just
            newCapture = maybe groupCapture (Just . maybe id (++) groupCapture) capture
            newMatched = groupMatched ++ matched
    matchToEnd match@(Just ("",_)) = match
    matchToEnd _ = Nothing

regex :: [String] -> String -> String -> String
regex exprNames str pattern
  | null str = findMatch exprNames
  | otherwise = findMatch (substrings str)
  where
    pat = parseRegex pattern
    findMatch [] = ""
    findMatch (str:strs) = maybe (findMatch strs) id (matchRegex str pat)

substrings :: String -> [String]
substrings str = substrs 1 str
  where
    substrs len s
      | len <= length s = take len s : substrs len (drop 1 s)
      | len < length str = substrs (len+1) str
      | otherwise = []

eval :: [String] -> String -> [Expr] -> [String] -> String
eval exprNames exprSrc [] [result] = result
eval exprNames exprSrc (ExprLiteral s:exprs) stack = eval exprNames exprSrc exprs (s:stack)
eval exprNames exprSrc (ExprOperator op:exprs) (op1:op2:stack) = eval exprNames exprSrc exprs (op exprNames op1 op2:stack)
eval _ exprSrc _ _ = error ("Invalid expression:"++exprSrc)

interp :: [(String,(String,[Expr]))] -> [(String,(String,[Expr]))] -> String
interp [] [(name,_)] = name
interp past [] = interp [] (reverse past)
interp past ((name,item@(exprSrc,exprs)):future)
  | null newName = interp past future
  | otherwise = flow past future
  where
    -- From https://github.com/graue/esofiles/blob/master/sortle/impl/sortle.pl
    -- # The .pdf spec doesn't specify an order, but the earlier sortle.txt
    -- # spec says expression names are searched in reverse order, starting
    -- # with the one before the current expression.
    exprNames = (map fst (past ++ reverse future))
    newName = eval exprNames exprSrc exprs []
    flow past (futureItem@(futureName,_):future)
      | newName == futureName = interp ((newName,item):past) future
      | newName > futureName = flow (futureItem:past) future
    flow past@(pastItem@(pastName,_):pasts) future
      | newName == pastName = interp ((newName,item):pasts) future
      | newName < pastName = flow pasts (pastItem:future)
    flow past future = interp ((newName,item):past) future

run :: String -> String
run prog = interp [] (parse prog)

sortle :: String -> IO ()
sortle prog = putStr (run prog)

main :: IO ()
main = getArgs >>= readFile . head >>= sortle
