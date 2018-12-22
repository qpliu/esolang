-- https://esolangs.org/wiki/Funciton

import Data.Array(Array,array,assocs,bounds,inRange,(!))
import Data.Map(Map)
import qualified Data.Map

data Dir = N | S | E | W deriving (Eq,Show)

data BoxType = Box0 | Box1 | Box2Opp | Box2Adj | Box3 | Box4 | Tee | Cross deriving (Eq,Show)

data RawBox = RawBox {
    rawBoxType :: BoxType,
    rawNW, rawSE :: (Int,Int),
    rawN, rawS, rawE, rawW :: Maybe (Int,Int)
    } deriving Show

data Box = Box {
    boxType :: BoxType,
    boxId :: (Int,Int),
    boxPrivate :: Bool,
    boxContents :: String,
    boxN, boxS, boxE, boxW :: BoxConnection
    } deriving Show

data BoxConnection = BoxConn Dir (Int,Int) | BoxConnExit Dir | BoxConnNone deriving Show

rotate :: Dir -> Dir
rotate N = E
rotate S = W
rotate E = S
rotate W = N

parse :: String -> [Box] -- undefined
parse prog = boxes -- undefined
  where
    grid :: Array (Int,Int) Char
    grid = array ((1,1),(width,height)) (concat (zipWith makeGridLine [1..] (lines prog)))
      where
        width = maximum (map length (lines prog))
        height = length (lines prog)
        makeGridLine y line = zip (map (flip (,) y) [1..]) (take width (line ++ repeat ' '))

    -- ─ │ ═ ║ ╔ ╗ ╚ ╝ ┌ ┐ └ ┘ ╓ ╖ ╙ ╜ ╒ ╕ ╘ ╛ ├ ┴ ┬ ┤ ┼ ╟ ╧ ╤ ╢ ╫ ╪
    connects :: Char -> Dir -> Char -> Bool
    connects '─' E c = c `elem` "─┐┘╖╜┴┬┤┼╢╫"
    connects '─' W c = c `elem` "─┌└╓╙┴┬├┼╟╫"
    connects '│' N c = c `elem` "│┌┐╒╕├┬┤┼╤╪"
    connects '│' S c = c `elem` "│└┘╘╛├┴┤┼╧╪"
    connects '═' E c = c `elem` "═╗╝╕╛╧╤╪"
    connects '═' W c = c `elem` "═╔╚╒╘╛╧╤╪"
    connects '║' N c = c `elem` "║╔╗╓╖╟╢╫"
    connects '║' S c = c `elem` "║╚╝╙╜╟╢╫"
    connects '╔' S c = c `elem` "║╚╝╙╜╟╢"
    connects '╔' E c = c `elem` "═╗╝╕╛╧╤╪"
    connects '╗' S c = c `elem` "║╚╝╙╜╟╢╫"
    connects '╗' W c = c `elem` "═╔╚╒╘╧╤╪"
    connects '╚' N c = c `elem` "║╔╗╓╖╟╢╫"
    connects '╚' E c = c `elem` "═╗╝╕╛╧╤╪"
    connects '╝' N c = c `elem` "║╔╗╓╖╟╢╫"
    connects '╝' W c = c `elem` "═╔╚╒╘╧╤╪"
    connects '┌' S c = c `elem` "│└┘╘╛├┴┤┼╧╪"
    connects '┌' E c = c `elem` "─┐┘╖╜┴┬┤┼╢╫"
    connects '┐' S c = c `elem` "│└┘╘╛├┴┤┼╧╪"
    connects '┐' W c = c `elem` "─┌└╓╙┴┬├┼╟╫"
    connects '└' N c = c `elem` "│┌┐╒╕├┬┤┼╤╪"
    connects '└' E c = c `elem` "─┐┘╖╜┴┬┤┼╢╫"
    connects '┘' N c = c `elem` "│┌┐╒╕├┬┤┼╤╪"
    connects '┘' W c = c `elem` "─┌└╓╙┴┬├┼╟╫"
    connects '╓' S c = c `elem` "║╚╝╙╜╟╢╫"
    connects '╓' E c = c `elem` "─┐┘╖╜┴┬┤┼╢"
    connects '╖' S c = c `elem` "║╚╝╙╜╟╢╫"
    connects '╖' W c = c `elem` "─┌└╓╙┴┬├┼╟"
    connects '╙' N c = c `elem` "║╔╗╓╖╟╢╫"
    connects '╙' E c = c `elem` "─┐┘╖╜┴┬┤┼╢"
    connects '╜' N c = c `elem` "║╔╗╓╖╟╢╫"
    connects '╜' W c = c `elem` "─┌└╓╙┴┬├┼╟"
    connects '╒' S c = c `elem` "│└┘╘╛├┴┤┼╧╪"
    connects '╒' E c = c `elem` "═╗╝╕╛╧╤╫╪"
    connects '╕' S c = c `elem` "│└┘╘╛├┴┤┼╧╪"
    connects '╕' W c = c `elem` "═╔╚╒╘╛╧╤╪"
    connects '╘' N c = c `elem` "│┌┐╒╕├┬┤┼╤╪"
    connects '╘' E c = c `elem` "═╗╝╕╛╧╤╪"
    connects '╛' N c = c `elem` "│┌┐╒╕├┬┤┼╤╪"
    connects '╛' W c = c `elem` "═╔╚╒╘╛╧╤╪"
    connects '├' N c = c `elem` "│┌┐╒╕├┬┤┼╤╪"
    connects '├' S c = c `elem` "│└┘╘╛├┴┤┼╧╪"
    connects '├' E c = c `elem` "─┐┘╖╜┴┬┤┼╢╫"
    connects '┴' N c = c `elem` "│┌┐╒╕├┬┤┼╤╪"
    connects '┴' E c = c `elem` "─┐┘╖╜┴┬┤┼╢╫"
    connects '┴' W c = c `elem` "─┌└╓╙┴┬├┼╟╫"
    connects '┬' S c = c `elem` "│└┘╘╛├┴┤┼╧╪"
    connects '┬' E c = c `elem` "─┐┘╖╜┴┬┤┼╢╫"
    connects '┬' W c = c `elem` "─┌└╓╙┴┬├┼╟╫"
    connects '┤' N c = c `elem` "│┌┐╒╕├┬┤┼╤╪"
    connects '┤' S c = c `elem` "│└┘╘╛├┴┤┼╧╪"
    connects '┤' W c = c `elem` "─┌└╓╙┴┬├┼╟╫"
    connects '┼' N c = c `elem` "│┌┐╒╕├┬┤┼╤╪"
    connects '┼' S c = c `elem` "│└┘╘╛├┴┤┼╧╪"
    connects '┼' E c = c `elem` "─┐┘╖╜┴┬┤┼╢╫"
    connects '┼' W c = c `elem` "─┌└╓╙┴┬├┼╟╫"
    connects '╟' N c = c `elem` "║╔╗╓╖╟╢╫"
    connects '╟' S c = c `elem` "║╚╝╙╜╟╢╫"
    connects '╟' E c = c `elem` "─┐┘╖╜┴┬┤┼╢╫"
    connects '╧' N c = c `elem` "│┌┐╒╕├┬┤┼╤╪"
    connects '╧' E c = c `elem` "═╗╝╕╛╧╤╪"
    connects '╧' W c = c `elem` "═╔╚╒╘╛╧╤╪"
    connects '╤' S c = c `elem` "│└┘╘╛├┴┤┼╧╪"
    connects '╤' E c = c `elem` "═╗╝╕╛╧╤╪"
    connects '╤' W c = c `elem` "═╔╚╒╘╛╧╤╪"
    connects '╢' N c = c `elem` "║╔╗╓╖╟╢╫"
    connects '╢' S c = c `elem` "║╚╝╙╜╟╢╫"
    connects '╢' W c = c `elem` "─┌└╓╙┴┬├┼╟╫"
    connects '╫' N c = c `elem` "║╔╗╓╖╟╢╫"
    connects '╫' S c = c `elem` "║╚╝╙╜╟╢╫"
    connects '╫' E c = c `elem` "─┐┘╖╜┴┬┤┼╢╫"
    connects '╫' W c = c `elem` "─┌└╓╙┴┬├┼╟╫"
    connects '╪' N c = c `elem` "│┌┐╒╕├┬┤┼╤╪"
    connects '╪' S c = c `elem` "│└┘╘╛├┴┤┼╧╪"
    connects '╪' E c = c `elem` "═╗╝╕╛╧╤╪"
    connects '╪' W c = c `elem` "═╔╚╒╘╛╧╤╪"
    connects _ _ _ = False

    move :: (Int,Int) -> Dir -> (Int,Int)
    move (x,y) N = (x,y-1)
    move (x,y) S = (x,y+1)
    move (x,y) E = (x+1,y)
    move (x,y) W = (x-1,y)

    connected :: (Int,Int) -> Dir -> Bool
    connected pos dir
      | not (inRange (bounds grid) (move pos dir)) = False
      | otherwise = connects (grid!pos) dir (grid!(move pos dir))

    leads :: (Int,Int) -> Dir -> Bool
    leads pos dir
      | dir == N || dir == S = connects (grid!pos) dir '│'
      | dir == E || dir == W = connects (grid!pos) dir '─'

    exits :: (Int,Int) -> Dir -> Bool
    exits pos dir = not (connected pos dir) && leads pos dir

    rawBoxes :: [RawBox]
    rawBoxes = filter (not . isComment) (filterOverlaps (foldl findRawBoxes [] (assocs grid)))

    findRawBoxes :: [RawBox] -> ((Int,Int),Char) -> [RawBox]
    findRawBoxes rawBoxes (pos,ch)
      | ch == '╔' = traceRawBoxTop pos pos [N] Nothing rawBoxes
      | ch == '┌' = traceRawBoxTop pos pos [] Nothing rawBoxes
      | ch == '╓' = traceRawBoxTop pos pos [] Nothing rawBoxes
      | ch == '╒' = traceRawBoxTop pos pos [N] Nothing rawBoxes
      | ch == '├' = RawBox{rawBoxType=Tee,rawNW=pos,rawSE=pos,rawN=Just pos,rawS=Just pos,rawE=Just pos,rawW=Nothing}:rawBoxes
      | ch == '┴' = RawBox{rawBoxType=Tee,rawNW=pos,rawSE=pos,rawN=Just pos,rawS=Nothing,rawE=Just pos,rawW=Just pos}:rawBoxes
      | ch == '┬' = RawBox{rawBoxType=Tee,rawNW=pos,rawSE=pos,rawN=Nothing,rawS=Just pos,rawE=Just pos,rawW=Just pos}:rawBoxes
      | ch == '┤' = RawBox{rawBoxType=Tee,rawNW=pos,rawSE=pos,rawN=Just pos,rawS=Just pos,rawE=Nothing,rawW=Just pos}:rawBoxes
      | ch == '┼' = RawBox{rawBoxType=Cross,rawNW=pos,rawSE=pos,rawN=Just pos,rawS=Just pos,rawE=Just pos,rawW=Just pos}:rawBoxes
      | otherwise = rawBoxes

    traceRawBoxTop :: (Int,Int) -> (Int,Int) -> [Dir] -> Maybe (Int,Int) -> [RawBox] -> [RawBox]
    traceRawBoxTop pos0 pos@(x,y) doubles nOut rawBoxes
      | leads pos N && connected pos E = maybe (traceRawBoxTop pos0 (move pos E) doubles (Just pos) rawBoxes) (const rawBoxes) nOut
      | connected pos E = traceRawBoxTop pos0 (move pos E) doubles nOut rawBoxes
      | (grid!pos) `elem` "╗╖" = traceRawBoxRight (pos0,x) pos (E:doubles) nOut Nothing rawBoxes
      | (grid!pos) `elem` "┐╕" = traceRawBoxRight (pos0,x) pos doubles nOut Nothing rawBoxes
      | otherwise = rawBoxes

    traceRawBoxRight :: ((Int,Int),Int) -> (Int,Int) -> [Dir] -> Maybe (Int,Int) -> Maybe (Int,Int) -> [RawBox] -> [RawBox]
    traceRawBoxRight (pos0,x1) pos@(x,y) doubles nOut eOut rawBoxes
      | leads pos E && connected pos S = maybe (traceRawBoxRight (pos0,x1) (move pos S) doubles nOut (Just pos) rawBoxes) (const rawBoxes) eOut
      | connected pos S = traceRawBoxRight (pos0,x1) (move pos S) doubles nOut eOut rawBoxes
      | (grid!pos) `elem` "╝╛" = traceRawBoxBottom (pos0,(x1,y)) pos (S:doubles) nOut eOut Nothing rawBoxes
      | (grid!pos) `elem` "┘╜" = traceRawBoxBottom (pos0,(x1,y)) pos doubles nOut eOut Nothing rawBoxes
      | otherwise = rawBoxes

    traceRawBoxBottom :: ((Int,Int),(Int,Int)) -> (Int,Int) -> [Dir] -> Maybe (Int,Int) -> Maybe (Int,Int) -> Maybe (Int,Int) -> [RawBox] -> [RawBox]
    traceRawBoxBottom (pos0@(x0,y0),pos1) pos@(x,y) doubles nOut eOut sOut rawBoxes
      | x < x0 = rawBoxes
      | leads pos S && connected pos W = maybe (traceRawBoxBottom (pos0,pos1) (move pos W) doubles nOut eOut (Just pos) rawBoxes) (const rawBoxes) sOut
      | connected pos W = traceRawBoxBottom (pos0,pos1) (move pos W) doubles nOut eOut sOut rawBoxes
      | x /= x0 = rawBoxes
      | (grid!pos) `elem` "╚╙" = traceRawBoxLeft (pos0,pos1) pos (W:doubles) nOut eOut sOut Nothing rawBoxes
      | (grid!pos) `elem` "└╘" = traceRawBoxLeft (pos0,pos1) pos doubles nOut eOut sOut Nothing rawBoxes
      | otherwise = rawBoxes

    traceRawBoxLeft :: ((Int,Int),(Int,Int)) -> (Int,Int) -> [Dir] -> Maybe (Int,Int) -> Maybe (Int,Int) -> Maybe (Int,Int) -> Maybe (Int,Int) -> [RawBox] -> [RawBox]
    traceRawBoxLeft (pos0@(_,y0),pos1) pos@(_,y) doubles nOut eOut sOut wOut rawBoxes
      | null doubles = rawBoxes
      | y == y0 = RawBox{rawBoxType=makeRawBoxType doubles,rawNW=pos0,rawSE=pos1,rawN=nOut,rawS=sOut,rawE=eOut,rawW=wOut} : rawBoxes
      | leads pos W && connected pos N = maybe (traceRawBoxLeft (pos0,pos1) (move pos N) doubles nOut eOut sOut (Just pos) rawBoxes) (const rawBoxes) wOut
      | connected pos N = traceRawBoxLeft (pos0,pos1) (move pos N) doubles nOut eOut sOut wOut rawBoxes
      | otherwise = rawBoxes

    makeRawBoxType :: [Dir] -> BoxType
    makeRawBoxType [] = Box0
    makeRawBoxType [_] = Box1
    makeRawBoxType [S,N] = Box2Opp
    makeRawBoxType [W,E] = Box2Opp
    makeRawBoxType [E,N] = Box2Adj
    makeRawBoxType [S,E] = Box2Adj
    makeRawBoxType [W,S] = Box2Adj
    makeRawBoxType [N,W] = Box2Adj
    makeRawBoxType [_,_,_] = Box3
    makeRawBoxType _ = Box4

    filterOverlaps :: [RawBox] -> [RawBox]
    filterOverlaps [] = []
    filterOverlaps (rawBox@RawBox{rawNW=nw}:rawBoxes)
      | any overlaps rawBoxes = filterOverlaps rawBoxes
      | otherwise = rawBox : filterOverlaps rawBoxes
      where
        overlaps RawBox{rawNW=nw0,rawSE=se0} = inRange (nw0,se0) nw

    isComment :: RawBox -> Bool
    isComment RawBox{rawN=Nothing,rawS=Nothing,rawE=Nothing,rawW=Nothing} = True
    isComment _ = False

    rawOutlets :: Map (Int,Int) (Int,Int)
    rawOutlets = Data.Map.fromList (concatMap outlets rawBoxes)
      where
        outlets RawBox{rawNW=boxId,rawN=n,rawS=s,rawE=e,rawW=w} = (addOutlet n . addOutlet s . addOutlet e . addOutlet w) []
          where
            addOutlet pos list = maybe list ((:list) . flip (,) boxId) pos

    trim :: String -> String
    trim str = (reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')) str

    boxes :: [Box]
    boxes = map makeBox rawBoxes

    makeBox :: RawBox -> Box
    makeBox RawBox{rawBoxType=boxType,rawNW=boxId@(x0,y0),rawSE=(x1,y1),rawN=n,rawS=s,rawE=e,rawW=w} =
        Box {
            boxType = boxType,
            boxId = boxId,
            boxPrivate = boxPrivate,
            boxContents = (if boxPrivate then trim . tail else id) boxContents,
            boxN = maybe BoxConnNone (traceConn N) n,
            boxS = maybe BoxConnNone (traceConn S) s,
            boxE = maybe BoxConnNone (traceConn E) e,
            boxW = maybe BoxConnNone (traceConn W) w
            }
      where
        boxPrivate = boxType == Box2Opp && (grid!(x0+1,y0+1)) == '┘' && connected (x0+1,y0+1) W && connected (x0+1,y0+1) N
        boxContents = concat [trim [grid!(x,y) | x <- [x0+1..x1-1]] | y <- [y0+1..y1-1]]

    traceConn :: Dir -> (Int,Int) -> BoxConnection
    traceConn dir pos = continue dir pos
      where
        trace dir pos = maybe (continue dir pos) (BoxConn dir) (Data.Map.lookup pos rawOutlets)
        continue dir pos
          | exits pos dir = BoxConnExit dir
          | connected pos dir = trace dir (move pos dir)
          | exits pos (rotate dir) = BoxConnExit (rotate dir)
          | connected pos (rotate dir) = trace (rotate dir) (move pos (rotate dir))
          | exits pos (rotate $ rotate $ rotate dir) = BoxConnExit (rotate $ rotate $ rotate dir)
          | connected pos (rotate $ rotate $ rotate dir) = trace (rotate $ rotate $ rotate dir) (move pos (rotate $ rotate $ rotate dir))
          | otherwise = error ("wtf@" ++ show pos)
