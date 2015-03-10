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
  | startCol < minCol || name == ' ' = (minCol,partials)
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
    bodyElems = (concat . zipWith enumBodyLine [0..] . reverse) body
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

data Dir = Up | Dn | Lt | Rt deriving (Eq,Show)
type Orientation = (Dir,Bool)

-- Rotate = 90 degrees clockwise
-- Flip = horizontal flip around vertical axis (i.e. 180 degree rotation through the 3rd dimension)
data XForm = Rotate | Flip | XForm [XForm]

-- Should reduce to no more than 1 flip and no more than 3 rotates
reduce :: XForm -> XForm
reduce = xf . xform (Up,False)
  where
    xf (Up,False) = XForm []
    xf (Rt,False) = Rotate
    xf (Dn,False) = XForm [Rotate,Rotate]
    xf (Lt,False) = XForm [Rotate,Rotate,Rotate]
    xf (Up,True)  = Flip
    xf (Rt,True)  = XForm [Flip,Rotate]
    xf (Dn,True)  = XForm [Flip,Rotate,Rotate]
    xf (Lt,True)  = XForm [Flip,Rotate,Rotate,Rotate]

xform :: Orientation -> XForm -> Orientation
xform (Up,False) Rotate = (Rt,False)
xform (Up,False) Flip   = (Up,True)
xform (Rt,False) Rotate = (Dn,False)
xform (Rt,False) Flip   = (Lt,True)
xform (Dn,False) Rotate = (Lt,False)
xform (Dn,False) Flip   = (Dn,True)
xform (Lt,False) Rotate = (Up,False)
xform (Lt,False) Flip   = (Rt,True)
xform (Up,True)  Rotate = (Rt,True)
xform (Up,True)  Flip   = (Up,False)
xform (Rt,True)  Rotate = (Dn,True)
xform (Rt,True)  Flip   = (Lt,False)
xform (Dn,True)  Rotate = (Lt,True)
xform (Dn,True)  Flip   = (Dn,False)
xform (Lt,True)  Rotate = (Up,True)
xform (Lt,True)  Flip   = (Rt,False)
xform orientation (XForm xforms) = foldl xform orientation xforms

inverse :: XForm -> XForm
inverse = inv . reduce
  where
    inv Rotate = XForm [Rotate,Rotate,Rotate]
    inv (XForm [Rotate,Rotate,Rotate]) = Rotate
    inv xform = xform

(&) :: XForm -> XForm -> XForm
x & y = (reduce . XForm) [x,y]

synonyms :: Char -> [(Char,XForm)]
synonyms '(' = [(')',Flip)]
synonyms ')' = [('(',Flip)]
synonyms '[' = [(']',Flip)]
synonyms ']' = [('[',Flip)]
synonyms '{' = [('}',Flip)]
synonyms '}' = [('{',Flip)]
synonyms '^' = [('>',Rotate),('v',Rotate & Rotate),('<',Rotate & Rotate & Rotate)]
synonyms '>' = [('v',Rotate),('<',Rotate & Rotate),('^',Rotate & Rotate & Rotate)]
synonyms 'v' = [('<',Rotate),('^',Rotate & Rotate),('>',Rotate & Rotate & Rotate)]
synonyms '<' = [('^',Rotate),('>',Rotate & Rotate),('v',Rotate & Rotate & Rotate)]
synonyms '6' = [('9',Rotate & Rotate)]
synonyms '9' = [('6',Rotate & Rotate)]
synonyms 'u' = [('n',Rotate & Rotate)]
synonyms 'n' = [('u',Rotate & Rotate)]
synonyms 'd' = [('q',Rotate & Rotate & Flip),('p',Rotate & Rotate),('b',Flip)]
synonyms 'q' = [('d',Rotate & Rotate & Flip),('b',Rotate & Rotate),('p',Flip)]
synonyms 'p' = [('b',Rotate & Rotate & Flip),('d',Rotate & Rotate),('q',Flip)]
synonyms 'b' = [('p',Rotate & Rotate & Flip),('q',Rotate & Rotate),('d',Flip)]
synonyms 'N' = [('Z',Rotate)]
synonyms 'Z' = [('N',Rotate & Rotate & Rotate)]
synonyms '|' = [('-',Rotate)]
synonyms '-' = [('|',Rotate & Rotate & Rotate)]
synonyms _ = []

unparse :: (Char,Proc) -> String
unparse (name,Proc{
    procEntT = entT,
    procEntR = entR,
    procEntB = entB,
    procEntL = entL,
    procBody = body
    }) =
    unlines (vborder entT : map procLine [0..maxRow] ++ [vborder entB])
  where
    (_,(maxCol,maxRow)) = A.bounds body
    border ent i = if maybe False (i ==) ent then '+' else ' '
    vborder ent = name : map (border ent) [0..maxCol] ++ [name]
    procLine row =
        border entL row : map ((body A.!) . (flip (,) row)) [0..maxCol]
                ++ [border entR row]

data Frame = Frame {
    frameCaller :: Maybe Frame,
    framePos    :: (Int,Int),
    frameDir    :: Dir,
    frameCells  :: Array (Int,Int) Cell
    }

data Cell = Cell Orientation Insn

type Insn = Map Char Proc -> Frame -> [Data] -> (Maybe Frame,[Data],Maybe Data)

data Data = DDir Dir | DRot Bool | DFlip Bool | DRet Dir deriving Show


insnFlip :: Insn
insnFlip procs frame inp = undefined

insnTurn :: Insn
insnTurn procs frame@Frame{frameDir = dir} inp
  | dir == Dn = (Just frame{frameDir = Lt},inp,Nothing)
  | dir == Rt = (Just frame{frameDir = Dn},inp,Nothing)
  | dir == Up = (Just frame{frameDir = Rt},inp,Nothing)
  | dir == Lt = (Just frame{frameDir = Up},inp,Nothing)

insnBackturn :: Insn
insnBackturn procs frame@Frame{frameDir = dir} inp
  | dir == Dn = (Just frame{frameDir = Rt},inp,Nothing)
  | dir == Rt = (Just frame{frameDir = Up},inp,Nothing)
  | dir == Up = (Just frame{frameDir = Lt},inp,Nothing)
  | dir == Lt = (Just frame{frameDir = Dn},inp,Nothing)

insnInput :: Insn
insnInput procs frame@Frame{frameDir = dir} inp
  | null inp = (Just frame{frameDir = uturn dir},inp,Nothing)
  | otherwise = undefined

insnCheckStack :: Insn
insnCheckStack procs frame@Frame{frameCaller = caller, frameDir = dir} inp =
    (Just frame{frameDir = maybe (uturn dir) (const dir) caller},inp,Nothing)

insnCall :: Orientation -> Proc -> Insn
insnCall = undefined

uturn :: Dir -> Dir
uturn dir = fst (xform (dir,True) (Rotate & Rotate))

interp :: Map Char Proc -> [Data] -> [Data]
interp = undefined

roag :: String -> [Data] -> [Data]
roag = interp . parse
