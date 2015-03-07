import Data.Array(Array)
import qualified Data.Array as A
import Data.Map(Map)
import qualified Data.Map as M

data Proc = Proc {
    procEntU, procEntR, procEntD, procEntL :: Maybe Int,
    procBody :: Array (Int,Int) Char
    }
  deriving Show

data Partial = Partial {
    partialName :: Char,
    partialStartCol, partialEndCol :: Int,
    partialEntU, partialEntR, partialEntL :: Maybe Int,
    partialBody :: [String]
    }

parse :: String -> Map Char Proc
parse prog
  | null partials = procs
  | otherwise = error "parse error: vertically unterminated definition(s)"
  where
    (partials,procs) = (foldl parseLine ([],M.empty) . lines) prog

parseLine :: ([Partial],Map Char Proc) -> String -> ([Partial],Map Char Proc)
parseLine (partials,procs) line =
    foldl (continuePartial chars) (newDefs,procs) partials
  where
    chars = take (maximum (length line : map ((+1) . partialEndCol) partials))
                 (line ++ repeat ' ')
    newDefs = map (startPartial chars)
                  (foldl filterDefs (zip chars [0..]) partials)
    filterDefs charCols Partial{partialStartCol = startCol, partialEndCol = endCol} =
        filter (not . A.inRange (startCol,endCol) . snd) charCols

startPartial :: String -> (Char,Int) -> Partial
startPartial line (name,startCol)
  | null trailer = error "parse error: horizontally unterminated definition"
  | otherwise = Partial{
        partialName = name,
        partialStartCol = startCol,
        partialEndCol = (snd . head) trailer,
        partialEntU = entry,
        partialEntR = Nothing,
        partialEntL = Nothing,
        partialBody = []
        }
  where
    (border,trailer) =
        break ((== name) . fst) (drop (startCol + 1) (zip line [0..]))
    borderEntries =
        map snd $ filter ((/= ' ') . fst) $ zip (map fst border) [0..]
    entry | null borderEntries = Nothing
          | length borderEntries > 1 =
                error "parse error: multiple top entries"
          | otherwise = (Just . head) borderEntries

continuePartial :: String -> ([Partial],Map Char Proc) -> Partial -> ([Partial],Map Char Proc)
continuePartial line (partials,procs) partial@Partial{
        partialName = name,
        partialStartCol = startCol,
        partialEndCol = endCol,
        partialEntU = entU,
        partialEntR = entR,
        partialEntL = entL,
        partialBody = body
        }
  | any (flip M.member procs) (name:(map fst . synonyms) name) =
        error ("error: multiply defined: " ++ show name)
  | lborder == name = (partials,M.insert name finishProc procs)
  | otherwise = (partial{partialEntR = rent, partialEntL = lent, partialBody = bodyline:body}:partials,procs)
  where
    (leftLine,rborder:_) = splitAt endCol line
    lborder:bodyline = drop startCol leftLine
    lent | lborder == ' ' = entL
         | otherwise = maybe (Just (length body))
                             (error "parse error: multiple left entries") entL
    rent | rborder == ' ' = entR
         | otherwise = maybe (Just (length body))
                             (error "parse error: multiple right entries") entR
    borderEntries =
        map snd $ filter ((/= ' ') . fst) $ zip bodyline [0..]
    entry | null borderEntries = Nothing
          | length borderEntries > 1 =
                error "parse error: multiple bottom entries"
          | otherwise = (Just . head) borderEntries
    finishProc = Proc{
        procEntU = entU,
        procEntR = entR,
        procEntL = entL,
        procEntD = entry,
        procBody = undefined
        }

-- Rotate = 90 degrees clockwise
-- Flip = horizontal flip around vertical axis (i.e. 180 degree rotation through the 3rd dimension)
data XForm = Rotate | Flip | XForm [XForm]

-- Should reduce to no more than 1 flip and no more than 3 rotates
reduce :: XForm -> XForm
reduce = undefined

synonyms :: Char -> [(Char,XForm)]
synonyms '(' = [(')',Flip)]
synonyms ')' = [('(',Flip)]
synonyms '[' = [(']',Flip)]
synonyms ']' = [('[',Flip)]
synonyms '{' = [('}',Flip)]
synonyms '}' = [('{',Flip)]
synonyms '^' = [('>',Rotate),('v',XForm [Rotate,Rotate]),('<',XForm [Rotate,Rotate,Rotate])]
synonyms '>' = [('v',Rotate),('<',XForm [Rotate,Rotate]),('^',XForm [Rotate,Rotate,Rotate])]
synonyms 'v' = [('<',Rotate),('^',XForm [Rotate,Rotate]),('>',XForm [Rotate,Rotate,Rotate])]
synonyms '<' = [('^',Rotate),('>',XForm [Rotate,Rotate]),('v',XForm [Rotate,Rotate,Rotate])]
synonyms '6' = [('9',XForm [Rotate,Rotate])]
synonyms '9' = [('6',XForm [Rotate,Rotate])]
synonyms 'u' = [('n',XForm [Rotate,Rotate])]
synonyms 'n' = [('u',XForm [Rotate,Rotate])]
synonyms 'd' = [('q',XForm [Rotate,Rotate,Flip]),('p',XForm [Rotate,Rotate]),('b',Flip)]
synonyms 'q' = [('d',XForm [Rotate,Rotate,Flip]),('b',XForm [Rotate,Rotate]),('p',Flip)]
synonyms 'p' = [('b',XForm [Rotate,Rotate,Flip]),('d',XForm [Rotate,Rotate]),('q',Flip)]
synonyms 'b' = [('p',XForm [Rotate,Rotate,Flip]),('q',XForm [Rotate,Rotate]),('d',Flip)]
synonyms 'N' = [('Z',Rotate)]
synonyms 'Z' = [('N',XForm [Rotate,Rotate,Rotate])]
synonyms '|' = [('-',Rotate)]
synonyms '-' = [('|',XForm [Rotate,Rotate,Rotate])]
synonyms _ = []
