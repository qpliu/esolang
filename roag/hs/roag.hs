import Data.Array(Array)
import qualified Data.Array as A
import Data.Either(lefts,rights)
import Data.List(intersperse)
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe(isNothing)

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

-- Rotate = 90 degrees clockwise
-- Flip = horizontal flip around vertical axis (i.e. 180 degree rotation through the 3rd dimension)
data XForm = Rotate | Flip | XForm [XForm]

-- Should reduce to no more than 1 flip and no more than 3 rotates
reduce :: XForm -> XForm
reduce = getTransform . transform (Up,False)
  where
    getTransform (Up,False) = XForm []
    getTransform (Rt,False) = Rotate
    getTransform (Dn,False) = XForm [Rotate,Rotate]
    getTransform (Lt,False) = XForm [Rotate,Rotate,Rotate]
    getTransform (Up,True)  = Flip
    getTransform (Rt,True)  = XForm [Flip,Rotate]
    getTransform (Dn,True)  = XForm [Flip,Rotate,Rotate]
    getTransform (Lt,True)  = XForm [Flip,Rotate,Rotate,Rotate]

transform :: (Dir,Bool) -> XForm -> (Dir,Bool)
transform (Up,False) Rotate = (Rt,False)
transform (Up,False) Flip   = (Up,True)
transform (Rt,False) Rotate = (Dn,False)
transform (Rt,False) Flip   = (Lt,True)
transform (Dn,False) Rotate = (Lt,False)
transform (Dn,False) Flip   = (Dn,True)
transform (Lt,False) Rotate = (Up,False)
transform (Lt,False) Flip   = (Rt,True)
transform (Up,True)  Rotate = (Rt,True)
transform (Up,True)  Flip   = (Up,False)
transform (Rt,True)  Rotate = (Dn,True)
transform (Rt,True)  Flip   = (Lt,False)
transform (Dn,True)  Rotate = (Lt,True)
transform (Dn,True)  Flip   = (Dn,False)
transform (Lt,True)  Rotate = (Up,True)
transform (Lt,True)  Flip   = (Rt,False)
transform orientation (XForm xforms) = foldl transform orientation xforms

xform :: Dir -> XForm -> Dir
xform dir = fst . transform (dir,True)

isFlip :: XForm -> Bool
isFlip = snd . transform (Up,False)

inverse :: XForm -> XForm
inverse = inv . reduce
  where
    inv Rotate = XForm [Rotate,Rotate,Rotate]
    inv (XForm [Rotate,Rotate,Rotate]) = Rotate
    inv xf = xf

(&) :: XForm -> XForm -> XForm
x & y = (reduce . XForm) [x,y]

xf0 :: XForm
xf0 = XForm []

xf90 :: XForm
xf90 = Rotate

xf180 :: XForm
xf180 = XForm [Rotate,Rotate]

xf270 :: XForm
xf270 = XForm [Rotate,Rotate,Rotate]

xfhflip :: XForm
xfhflip = Flip

xfvflip :: XForm
xfvflip = XForm [Flip,Rotate,Rotate]

synonyms :: Char -> [(Char,XForm)]
synonyms '(' = [(')',Flip)]
synonyms ')' = [('(',Flip)]
synonyms '[' = [(']',Flip)]
synonyms ']' = [('[',Flip)]
synonyms '{' = [('}',Flip)]
synonyms '}' = [('{',Flip)]
synonyms '^' = [('>',xf90),('v',xf180),('<',xf270)]
synonyms '>' = [('v',xf90),('<',xf180),('^',xf270)]
synonyms 'v' = [('<',xf90),('^',xf180),('>',xf270)]
synonyms '<' = [('^',xf90),('>',xf180),('v',xf270)]
synonyms '6' = [('9',xf180)]
synonyms '9' = [('6',xf180)]
synonyms 'u' = [('n',xf180)]
synonyms 'n' = [('u',xf180)]
synonyms 'd' = [('q',xfvflip),('p',xf180),('b',xfhflip)]
synonyms 'q' = [('d',xfvflip),('b',xf180),('p',xfhflip)]
synonyms 'p' = [('b',xfvflip),('d',xf180),('q',xfhflip)]
synonyms 'b' = [('p',xfvflip),('q',xf180),('d',xfhflip)]
synonyms 'N' = [('Z',Rotate)]
synonyms 'Z' = [('N',xf270)]
synonyms '|' = [('-',Rotate)]
synonyms '-' = [('|',xf270)]
synonyms _ = []

canonical :: Char -> (Char,XForm)
canonical ')' = ('(',Flip)
canonical ']' = ('[',Flip)
canonical '}' = ('{',Flip)
canonical '>' = ('^',Rotate)
canonical 'v' = ('^',Rotate & Rotate)
canonical '<' = ('^',Rotate & Rotate & Rotate)
canonical '9' = ('6',Rotate & Rotate)
canonical 'u' = ('n',Rotate & Rotate)
canonical 'q' = ('d',Flip & Rotate & Rotate)
canonical 'p' = ('d',Rotate & Rotate)
canonical 'b' = ('d',Flip)
canonical 'Z' = ('N',Rotate)
canonical '-' = ('|',Rotate)
canonical '\\' = ('/',Rotate)
canonical c = (c,XForm [])

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

data Cell = Cell Char XForm Insn

type Insn = Map Char Proc -> Frame -> [Data] -> (Maybe Frame,[Data],[Data])

data Data = DDir Dir | DRot Bool | DFlip Bool | DRet Dir deriving Show

insnFlip :: Insn
insnFlip procs frame inp = undefined

insnTurn :: Insn
insnTurn procs frame@Frame{frameDir = dir} inp
  | dir == Dn = (Just frame{frameDir = Lt},inp,[])
  | dir == Rt = (Just frame{frameDir = Dn},inp,[])
  | dir == Up = (Just frame{frameDir = Rt},inp,[])
  | dir == Lt = (Just frame{frameDir = Up},inp,[])

insnBackturn :: Insn
insnBackturn procs frame@Frame{frameDir = dir} inp
  | dir == Dn = (Just frame{frameDir = Rt},inp,[])
  | dir == Rt = (Just frame{frameDir = Up},inp,[])
  | dir == Up = (Just frame{frameDir = Lt},inp,[])
  | dir == Lt = (Just frame{frameDir = Dn},inp,[])

insnInput :: Insn
insnInput procs frame@Frame{frameDir = dir} inp
  | null inp = (Just frame{frameDir = uturn dir},inp,[])
  | otherwise = undefined

insnCheckStack :: Insn
insnCheckStack procs frame@Frame{frameCaller = caller, frameDir = dir} inp =
    (Just frame{frameDir = maybe (uturn dir) (const dir) caller},inp,[])

insnCall :: Proc -> XForm -> Insn
insnCall = undefined

uturn :: Dir -> Dir
uturn dir = xform dir (Rotate & Rotate)

call :: Map Char Proc -> Maybe Frame -> Dir -> XForm -> Proc -> Frame
call prog caller callerDir callerXForm
     Proc{procEntT = entT, procEntR = entR, procEntB = entB, procEntL = entL,
          procBody = body} =
    Frame{frameCaller = caller, framePos = pos, frameDir = dir,
          frameCells = cells}
  where
    (_,(maxCol,maxRow)) = A.bounds body
    dir = xform callerDir (inverse callerXForm)
    pos | dir == Dn = maybe (0,maxRow) (flip (,) (-1)) entT
        | dir == Rt = maybe (maxCol,0) ((,) (-1)) entL
        | dir == Up = maybe (0,0) (flip (,) (maxRow+1)) entB
        | dir == Lt = maybe (0,0) ((,) (maxCol+1)) entR
    cells = A.array (A.bounds body) (map (fmap toCell) (A.assocs body))
    toCell ch = Cell ch cellXForm cellInsn
      where
        (cellXForm,cellInsn) = undefined

interp :: Map Char Proc -> Frame -> [Data] -> [Either Data String]
interp prog frame inp = (Right . visualize) frame : output ++ nextStep
  where
    advanceIP frame = undefined -- moving out means calling frame with direction adjustment
    (Just frame') = advanceIP frame
    (output,nextStep) | isNothing (advanceIP frame) = ([],[])
                      | otherwise = (insnOutput,interp prog frame'' inp'')
    (insnOutput,frame'',inp'') = undefined

visualize :: Frame -> String
visualize Frame{framePos = (x,y), frameDir = dir, frameCells = cells} =
    unlines (top : "" : concatMap body [0..maxRow] ++ [bottom])
  where
    (_,(maxCol,maxRow)) = A.bounds cells
    top = '*' : concatMap (vborder (-1)) [0..maxCol] ++ " *"
    bottom = '*' : concatMap (vborder (maxRow+1)) [0..maxCol] ++ " *"
    vborder y1 x1 = [' ',ip x1 y1,' ']
    ip x1 y1 | (x,y) /= (x1,y1) = ' ' | otherwise = showDir dir
    showDir dir | dir == Dn = 'v' | dir == Rt = '<'
                | dir == Up = '^' | dir == Lt = '>'
    body y1 = [' ' : concatMap (fst . cell y1) [0..maxCol],
               ip (-1) y1 : concatMap (snd . cell y1) [0..maxCol]
                   ++ [' ', ip (maxCol+1) y1],
               ""]
    cell y1 x1 = ([' ',cellFlip,cellCh],[' ',ip x1 y1,showDir cellDir])
      where
        (Cell cellCh cellXForm _) = cells A.! (x1,y1)
        cellFlip = if isFlip cellXForm then '~' else ' '
        cellDir = xform Up cellXForm

run :: Map Char Proc -> [Data] -> [Either Data String]
run prog = maybe (const []) (interp prog . call prog Nothing Dn (XForm []))
                 (M.lookup 'm' prog)

roag :: String -> [Data] -> [Data]
roag = (lefts .) . run . parse

roagVisualize :: String -> [Data] -> [String]
roagVisualize = (rights .) . run . parse

testVisualize :: String -> IO ()
testVisualize file = readFile file >>= sequence_ . intersperse (getLine >> return ()) . map putStr . flip roagVisualize []
