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

type Prog = Map Char (XForm,Proc)

parse :: String -> Prog
parse src
  | null partials = prog
  | otherwise = error "parse error: vertically unterminated definition(s)"
  where
    (partials,prog) = (foldl parseLine ([],M.empty) . lines) src

parseLine :: ([Partial],Prog) -> String -> ([Partial],Prog)
parseLine (partials,prog) line =
    foldl (continuePartial chars) (newDefs,prog) partials
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

continuePartial :: String -> ([Partial],Prog) -> Partial -> ([Partial],Prog)
continuePartial line (partials,prog) partial@Partial{
        partialName = name,
        partialStartCol = startCol,
        partialEndCol = endCol,
        partialEntT = entT,
        partialEntR = entR,
        partialEntL = entL,
        partialBody = body
        }
  | lborder /= name = (partial{partialEntR = rent, partialEntL = lent, partialBody = bodyline:body}:partials,prog)
  | isComment name = (partials,prog)
  | M.member canonName prog = error ("error: multiply defined: " ++ show name)
  | otherwise =
        (partials,M.insert canonName (inverse canonXForm,finishProc) prog)
  where
    (canonName,canonXForm) = canonical name
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

uncanonical :: (Char,XForm) -> Char
uncanonical ('(',Flip) = ')'
uncanonical ('[',Flip) = ']'
uncanonical ('{',Flip) = '}'
uncanonical ('^',Rotate) = '>'
uncanonical ('^',XForm [Rotate,Rotate]) = 'v'
uncanonical ('^',XForm [Rotate,Rotate,Rotate]) = '<'
uncanonical ('6',XForm [Rotate,Rotate]) = '9'
uncanonical ('n',XForm [Rotate,Rotate]) = 'u'
uncanonical ('d',XForm [Flip, Rotate,Rotate]) = 'q'
uncanonical ('d',XForm [Rotate,Rotate]) = 'p'
uncanonical ('d',Flip) = 'b'
uncanonical ('N',Rotate) = 'Z'
uncanonical ('|',Rotate) = '-'
uncanonical ('/',Rotate) = '\\'
uncanonical (c,XForm []) = c
uncanonical (c,_) = error ("no uncanonical transform of " ++ show c)

unparse :: (Char,(XForm,Proc)) -> String
unparse (procName,(procXForm,Proc{
    procEntT = entT,
    procEntR = entR,
    procEntB = entB,
    procEntL = entL,
    procBody = body
    })) =
    unlines (vborder entT : map procLine [0..maxRow] ++ [vborder entB])
  where
    name = uncanonical (procName,procXForm)
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

type Insn = Prog -> Frame -> XForm -> [Data] -> (Frame,[Data],[Data])

data Data = DDir Dir | DRot Bool | DFlip Bool | DRet Dir deriving (Eq,Show)

insnRotate :: Insn
insnRotate prog frame@Frame{framePos = (x,y), frameDir = dir,
                            frameCells = cells} cellXForm inp
  | dir == Dn && 2*x < maxCol = adjustCaller prog frame (DRot False) inp
  | dir == Dn && 2*x > maxCol = adjustCaller prog frame (DRot True) inp
  | dir == Rt && 2*y < maxRow = adjustCaller prog frame (DRot True) inp
  | dir == Rt && 2*y > maxRow = adjustCaller prog frame (DRot False) inp
  | dir == Up && 2*x < maxCol = adjustCaller prog frame (DRot True) inp
  | dir == Up && 2*x > maxCol = adjustCaller prog frame (DRot False) inp
  | dir == Lt && 2*y < maxRow = adjustCaller prog frame (DRot False) inp
  | dir == Lt && 2*y > maxRow = adjustCaller prog frame (DRot True) inp
  | otherwise = (frame,inp,[])
  where
    (_,(maxCol,maxRow)) = A.bounds cells

adjustCaller :: Prog -> Frame -> Data -> [Data] -> (Frame,[Data],[Data])
adjustCaller prog frame@Frame{frameCaller = caller} dat inp
  | isNothing caller = (frame,inp,[dat])
  | otherwise = case dat of
      DDir dir -> (moveCallerCell (xform dir cellXForm),inp,[])
      DRot cw -> (adjCallerCell ((if cw then xf90 else xf270) &),inp,[])
      DFlip horiz ->
        (adjCallerCell ((if horiz then xfhflip else xfvflip) &),inp,[])
      DRet dir -> (retCallerCell (xform dir cellXForm),inp,[])
  where
    Just callerFrame@Frame{framePos = pos@(x,y), frameCells = cells} = caller
    curCell@(Cell cellCh cellXForm cellInsn) = cells A.! pos
    adjCallerCell adj = frame{frameCaller = Just newCaller}
      where
        cell = Cell cellCh (adj cellXForm) cellInsn
        newCaller = callerFrame{frameCells = cells A.// [(pos,cell)]}
    newPos dir | dir == Dn = (x,y+1) | dir == Rt = (x+1,y)
               | dir == Up = (x,y-1) | dir == Lt = (x-1,y)
    moveCallerCell dir = frame{frameCaller = Just (insCell (newPos dir) dir)}
    retCallerCell dir = frame{frameCaller = Just (insCell (-1,-1) dir)}
    insCell insPos dir =
        insertCell curCell insPos dir
            callerFrame{frameCells = cells A.// [(pos,Cell ' ' xf0 insnNop)]}

insertCell :: Cell -> (Int,Int) -> Dir -> Frame -> Frame
insertCell cell pos dir frame@Frame{frameCells = cells}
  | not (A.inRange (A.bounds cells) pos) = undefined
  | otherwise = undefined

insnMove :: Insn
insnMove prog frame@Frame{frameDir = dir} cellXForm inp =
    adjustCaller prog frame (DDir dir) inp

insnFlip :: Insn
insnFlip prog frame@Frame{frameDir = dir} cellXForm inp =
    adjustCaller prog frame (DFlip (dir == Lt || dir == Rt)) inp

insnTurn :: Insn
insnTurn prog frame@Frame{frameDir = dir} cellXForm inp =
    (frame{frameDir = xform (turn cellDir) cellXForm},inp,[])
  where
    cellDir = xform dir (inverse cellXForm)
    turn Dn = Lt
    turn Rt = Up
    turn Up = Rt
    turn Lt = Dn

insnInput :: Insn
insnInput prog frame@Frame{frameDir = dir} cellXForm inp
  | null inp = (frame{frameDir = uturn dir},[],[])
  | otherwise = adjustCaller prog frame (head inp) (tail inp)

insnCheckStack :: Insn
insnCheckStack prog frame@Frame{frameCaller = caller, frameDir = dir} _ inp =
    (frame{frameDir = maybe (uturn dir) (const dir) caller},inp,[])

insnCall :: Proc -> Insn
insnCall proc prog frame@Frame{frameDir = dir} cellXForm inp =
    (call prog (Just frame) dir cellXForm proc,inp,[])

insnNop :: Insn
insnNop prog frame _ inp = (frame,inp,[])

uturn :: Dir -> Dir
uturn dir = xform dir (Rotate & Rotate)

call :: Prog -> Maybe Frame -> Dir -> XForm -> Proc -> Frame
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
    toCell procCh = Cell cellCh (procXForm & cellXForm) cellInsn
      where
        (cellCh,cellXForm) = canonical procCh
        (cellInsn,procXForm)
          | cellCh == ' ' = (insnNop,xf0)
          | cellCh == '@' = (insnRotate,xf0)
          | cellCh == '*' = (insnMove,xf0)
          | cellCh == '~' = (insnFlip,xf0)
          | cellCh == '/' = (insnTurn,xf0)
          | cellCh == '?' = (insnInput,xf0)
          | cellCh == '_' = (insnCheckStack,xf0)
          | otherwise = (insnCall proc,xf)
        (xf,proc) = maybe (error ("undefined procedure: " ++ show procCh))
                          id (M.lookup cellCh prog)

interp :: Prog -> Frame -> [Data] -> [Either Data String]
interp prog frame@Frame{frameCaller = caller, frameDir = dir,
                        framePos = pos@(x,y), frameCells = cells} inp =
    (Right . visualize) frame : map Left output ++ nextStep
  where
    (output,nextStep)
      | A.inRange (A.bounds cells) newPos = (output',interp prog frame' inp')
      | isNothing caller = ([],[])
      | otherwise = ([],interp prog retFrame inp)
    newPos | dir == Dn = (x,y+1) | dir == Rt = (x+1,y)
           | dir == Up = (x,y-1) | dir == Lt = (x-1,y)
    Just callerFrame@Frame{framePos = callerPos,
                           frameCells = callerCells} = caller
    retFrame = callerFrame{frameDir = xform dir callerXForm}
    Cell _ callerXForm _ = callerCells A.! callerPos

    (frame',inp',output') = insn prog frame{framePos = newPos} cellXForm inp
    Cell _ cellXForm insn = cells A.! newPos

visualize :: Frame -> String
visualize Frame{framePos = (x,y), frameDir = dir, frameCells = cells} =
    unlines (top : "" : concatMap body [0..maxRow] ++ [bottom])
  where
    (_,(maxCol,maxRow)) = A.bounds cells
    top = '*' : concatMap (vborder (-1)) [0..maxCol] ++ " *"
    bottom = '*' : concatMap (vborder (maxRow+1)) [0..maxCol] ++ " *"
    vborder y1 x1 = [' ',ip x1 y1,' ']
    ip x1 y1 | (x,y) /= (x1,y1) = ' ' | otherwise = showDir dir
    showDir dir | dir == Dn = 'v' | dir == Rt = '>'
                | dir == Up = '^' | dir == Lt = '<'
    body y1 = [' ' : concatMap (fst . cell y1) [0..maxCol],
               ip (-1) y1 : concatMap (snd . cell y1) [0..maxCol]
                   ++ [' ', ip (maxCol+1) y1],
               ""]
    cell y1 x1
        | cellCh `elem` "@*~?_" = ([' ',' ',cellCh],[' ',ip x1 y1,' '])
        | cellCh == '/' && (cellFlip == ' ') == (cellDir `elem` [Up,Dn]) =
                ("  /",[' ',ip x1 y1,' '])
        | cellCh == '/' = ("  \\",[' ',ip x1 y1,' '])
        | otherwise = ([' ',cellFlip,cellCh],[' ',ip x1 y1,showDir cellDir])
      where
        (Cell cellCh cellXForm _) = cells A.! (x1,y1)
        cellFlip = if isFlip cellXForm then '~' else ' '
        cellDir = xform Up cellXForm

run :: Prog -> [Data] -> [Either Data String]
run prog = maybe (const []) (interp prog . doCall) (M.lookup 'm' prog)
  where
    doCall (procXForm,proc) = call prog Nothing Dn (inverse procXForm) proc

roag :: String -> [Data] -> [Data]
roag = (lefts .) . run . parse

roagVisualize :: String -> [Data] -> [String]
roagVisualize = (rights .) . run . parse

testVisualize :: String -> [Data] -> IO ()
testVisualize file inp = readFile file >>= sequence_ . intersperse (getLine >> return ()) . map putStr . flip roagVisualize inp
