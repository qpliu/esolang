import Data.Array(Array)
import qualified Data.Array as A
import Data.Map(Map)
import qualified Data.Map as M

data Proc = Proc {
    procEntT, procEntR, procEntB, procEntL :: Maybe Int,
    procBody :: Array (Int,Int) Char
    }

data Partial = Partial {
    partialName :: Char,
    partialStartCol, partialEndCol :: Int,
    partialEntT, partialEntR, partialEntL :: Maybe Int,
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
    newDefs = (snd . foldl (startPartial chars) (0,[]))
                    (foldl filterDefs (zip chars [0..]) partials)
    filterDefs charCols Partial{partialStartCol = startCol, partialEndCol = endCol} =
        filter (not . A.inRange (startCol,endCol) . snd) charCols

startPartial :: String -> (Int,[Partial]) -> (Char,Int) -> (Int,[Partial])
startPartial line (minCol,partials) (name,startCol)
  | startCol < minCol = (minCol,partials)
  | null trailer = error "parse error: horizontally unterminated definition"
  | otherwise = (1+(snd . head) trailer,Partial{
        partialName = name,
        partialStartCol = startCol,
        partialEndCol = (snd . head) trailer,
        partialEntT = entry,
        partialEntR = Nothing,
        partialEntL = Nothing,
        partialBody = []
        }:partials)
  where
    (border,trailer) =
        break ((== name) . fst) (drop (startCol + 1) (zip line [0..]))
    borderEntries =
        map snd $ filter ((/= ' ') . fst) $ zip (map fst border) [0..]
    entry | null borderEntries || isComment name = Nothing
          | length borderEntries > 1 =
                error "parse error: multiple top entries"
          | otherwise = (Just . head) borderEntries

continuePartial :: String -> ([Partial],Map Char Proc) -> Partial -> ([Partial],Map Char Proc)
continuePartial line (partials,procs) partial@Partial{
        partialName = name,
        partialStartCol = startCol,
        partialEndCol = endCol,
        partialEntT = entT,
        partialEntR = entR,
        partialEntL = entL,
        partialBody = body
        }
  | lborder /= name = (partial{partialEntR = rent, partialEntL = lent, partialBody = bodyline:body}:partials,procs)
  | isComment name = (partials,procs)
  | any (flip M.member procs) (name:(map fst . synonyms) name) =
        error ("error: multiply defined: " ++ show name)
  | otherwise = (partials,M.insert name finishProc procs)
  where
    (leftLine,rborder:_) = splitAt endCol line
    lborder:bodyline = drop startCol leftLine
    lent | lborder == ' ' || isComment name = entL
         | otherwise = maybe (Just (length body))
                             (error "parse error: multiple left entries") entL
    rent | rborder == ' ' || isComment name = entR
         | otherwise = maybe (Just (length body))
                             (error "parse error: multiple right entries") entR
    borderEntries =
        map snd $ filter ((/= ' ') . fst) $ zip bodyline [0..]
    entry | null borderEntries || isComment name = Nothing
          | length borderEntries > 1 =
                error "parse error: multiple bottom entries"
          | otherwise = (Just . head) borderEntries
    finishProc = Proc{
        procEntT = entT,
        procEntR = entR,
        procEntL = entL,
        procEntB = entry,
        procBody = A.array (minimum bodyIxs,maximum bodyIxs) bodyElems
        }
    bodyElems = (concat . zipWith enumBodyLine [0..]) body
    enumBodyLine y line = zip (map (flip (,) y) [0..]) line
    bodyIxs = map fst bodyElems

isComment :: Char -> Bool
isComment '@' = True
isComment '*' = True
isComment '~' = True
isComment '/' = True
isComment '\\' = True
isComment '?' = True
isComment '_' = True
isComment _ = False

-- Rotate = 90 degrees clockwise
-- Flip = horizontal flip around vertical axis (i.e. 180 degree rotation through the 3rd dimension)
data XForm = Rotate | Flip | XForm [XForm]

-- Should reduce to no more than 1 flip and no more than 3 rotates
reduce :: XForm -> XForm
reduce = xf . xform 1234
  where
    xf 1234 = XForm []
    xf 3142 = Rotate
    xf 4321 = XForm [Rotate,Rotate]
    xf 2413 = XForm [Rotate,Rotate,Rotate]
    xf 2143 = Flip
    xf 4231 = XForm [Flip,Rotate]
    xf 3412 = XForm [Flip,Rotate,Rotate]
    xf 1324 = XForm [Flip,Rotate,Rotate,Rotate]
    xform 1234 Rotate = 3142
    xform 1234 Flip = 2143
    xform 3142 Rotate = 4321
    xform 3142 Flip = 1324
    xform 4321 Rotate = 2413
    xform 4321 Flip = 3412
    xform 2413 Rotate = 1234
    xform 2413 Flip = 4231
    xform 2143 Rotate = 4231
    xform 2143 Flip = 1234
    xform 4231 Rotate = 3412
    xform 4231 Flip = 2413
    xform 3412 Rotate = 1324
    xform 3412 Flip = 4321
    xform 1324 Rotate = 2143
    xform 1324 Flip = 3142
    xform b (XForm xforms) = foldl xform b xforms

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
