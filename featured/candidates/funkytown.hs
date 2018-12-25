-- https://esolangs.org/wiki/Funciton

-- Limitations:
--   If the counter-clockwise input of cross operator is not in
--   the range (minBound,maxBound) :: (Int,Int), then the clockwise
--   (shift-left) output will be wrong.
--   With ghc on my computer, the range is a signed 64-bit int
--   (-9223372036854775808,9223372036854775807).

import Data.Array(Array,array,assocs,bounds,inRange,(!))
import Data.Bits(complement,shift,(.&.))
import Data.Char(chr,ord)
import Data.Map(Map)
import qualified Data.Map
import Data.Set(Set)
import qualified Data.Set
import System.Environment(getArgs)

data Dir = N | S | E | W deriving (Eq,Ord,Show)

rotate :: Int -> Dir -> Dir
rotate 0 dir = dir
rotate 1 N = E
rotate 1 S = W
rotate 1 E = S
rotate 1 W = N
rotate 2 N = S
rotate 2 S = N
rotate 2 E = W
rotate 2 W = E
rotate 3 N = W
rotate 3 S = E
rotate 3 E = N
rotate 3 W = S
rotate n dir = rotate (n `mod` 4) dir

data BoxType = Box1 | Box2Opp | Box2Adj | Box3 | Box4 | Tee | Cross deriving (Eq,Show)

data Box = Box {
    boxType :: BoxType,
    boxId :: (Int,Int),
    boxPrivate :: Bool,
    boxContents :: String,
    boxN, boxS, boxE, boxW :: BoxConnection
    } deriving Show

data BoxConnection = BoxConn Dir (Int,Int) | BoxConnExit Dir | BoxConnNone deriving (Eq,Show)

data RawBox = RawBox {
    rawBoxType :: BoxType,
    rawNW, rawSE :: (Int,Int),
    rawN, rawS, rawE, rawW :: Maybe (Int,Int)
    } deriving Show

boxConn :: Box -> Dir -> BoxConnection
boxConn box N = boxN box
boxConn box S = boxS box
boxConn box E = boxE box
boxConn box W = boxW box

parseBoxes :: String -> [Box]
parseBoxes src = boxes
  where
    grid :: Array (Int,Int) Char
    grid = array ((1,1),(width,height)) (concat (zipWith makeGridLine [1..] (lines src)))
      where
        width = maximum (map length (lines src))
        height = length (lines src)
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
    makeRawBoxType [_] = Box1
    makeRawBoxType [S,N] = Box2Opp
    makeRawBoxType [W,E] = Box2Opp
    makeRawBoxType [E,N] = Box2Adj
    makeRawBoxType [S,E] = Box2Adj
    makeRawBoxType [W,S] = Box2Adj
    makeRawBoxType [N,W] = Box2Adj
    makeRawBoxType [_,_,_] = Box3
    makeRawBoxType [_,_,_,_] = Box4

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
        trace dir pos = maybe (continue dir pos) ((BoxConn . rotate 2) dir) (Data.Map.lookup pos rawOutlets)
        continue dir pos
          | exits pos dir = BoxConnExit dir
          | connected pos dir = trace dir (move pos dir)
          | exits pos (rotate 1 dir) = BoxConnExit (rotate 1 dir)
          | connected pos (rotate 1 dir) = trace (rotate 1 dir) (move pos (rotate 1 dir))
          | exits pos (rotate 3 dir) = BoxConnExit (rotate 3 dir)
          | connected pos (rotate 3 dir) = trace (rotate 3 dir) (move pos (rotate 3 dir))
          | otherwise = error ("wtf@" ++ show pos)

data Declaration = Declaration {
    declName :: Maybe String,
    declScope :: String,
    declPrivate :: Bool,
    declInputs :: Set Dir,
    declOutputs :: Set Dir
    } deriving Show

parseDeclarations :: (String,String) -> [(Declaration,Map (Int,Int) Box)]
parseDeclarations (scope,src) =
    [(makeDeclaration graph,graph) | graph <- distinctGraphs]
  where
    boxes :: Map (Int,Int) Box
    boxes = Data.Map.fromList [(boxId box,box) | box <- parseBoxes src]

    distinctGraphs :: [Map (Int,Int) Box]
    distinctGraphs = fst (Data.Map.fold collectDistinctGraphs ([],Data.Set.empty) boxes)

    collectDistinctGraphs :: Box -> ([Map (Int,Int) Box],Set (Int,Int)) -> ([Map (Int,Int) Box],Set (Int,Int))
    collectDistinctGraphs box (graphs,collected)
      | Data.Set.member (boxId box) collected = (graphs,collected)
      | otherwise = (graph:graphs,Data.Set.union collected (Data.Set.fromList (Data.Map.keys graph)))
      where
        graph = collect [BoxConn N (boxId box)] Data.Map.empty
        collect [] graph = graph
        collect (BoxConn _ bid:conns) graph
          | not (Data.Map.member bid graph) = collect (boxN b:boxS b:boxE b:boxW b:conns) (Data.Map.insert bid b graph)
          where b = boxes Data.Map.! bid
        collect (_:conns) graph = collect conns graph

    makeDeclaration :: Map (Int,Int) Box -> Declaration
    makeDeclaration graph = Data.Map.fold buildDeclaration Declaration{declName=Nothing,declScope=scope,declPrivate=False,declInputs=Data.Set.empty,declOutputs=Data.Set.empty} graph

    buildDeclaration :: Box -> Declaration -> Declaration
    buildDeclaration Box{boxType=boxType,boxPrivate=boxPrivate,boxContents=boxContents,boxN=n,boxS=s,boxE=e,boxW=w} decl =
        checkHeader (foldl buildOutput decl [n,s,e,w])
      where
        buildOutput decl@Declaration{declOutputs=outputs,declInputs=inputs} (BoxConnExit dir)
          | Data.Set.member dir outputs = error "Invalid function: duplicate output"
          | Data.Set.member dir inputs = error "Invalid function: input-output overlap"
          | otherwise = decl{declOutputs=Data.Set.insert dir outputs}
        buildOutput decl dir = decl
        checkHeader decl
          | boxType == Box2Opp = maybe (buildHeader decl) (const (error "Multiple declaration headers")) (declName decl)
          | otherwise = decl
        buildHeader decl@Declaration{declOutputs=outputs}
          | Data.Set.null (Data.Set.intersection outputs inputs) =
                decl{declName=Just boxContents,declPrivate=boxPrivate,declInputs=inputs}
          | otherwise = error "Invalid function: input-output overlap"
          where
            inputs = Data.Set.fromList (map fst (filter ((/= BoxConnNone) . snd) [(N,s),(S,n),(E,w),(W,e)]))

declarationErrors :: [Declaration] -> [String]
declarationErrors declarations = fst (foldl check ([],Data.Set.empty) declarations)
  where
    check (errs,seen) Declaration{declName=Just name,declScope=scope,declPrivate=private,declInputs=inputs,declOutputs=outputs}
      | inputs == Data.Set.fromList [N,S] && outputs == Data.Set.fromList [E,W] = (("Rotationally ambiguous:"++name):errs,seen) 
      | inputs == Data.Set.fromList [E,W] && outputs == Data.Set.fromList [N,S] = (("Rotationally ambiguous:"++name):errs,seen) 
      | private && Data.Set.member (Just scope,name) seen = (("Multiple definitions for:"++name):errs,seen)
      | private = (errs,Data.Set.insert (Just scope,name) seen)
      | Data.Set.member (Nothing,name) seen = (("Multiple definitions for:"++name):errs,seen)
      | otherwise = (errs,Data.Set.insert (Nothing,name) seen)
    check (dups,seen) _ = (dups,seen)

data FlowBoxType =
    FlowDecl
  | FlowConst (Maybe Integer)
  | FlowCall (Maybe String,String) (Set Dir) (Set Dir) -- name inputs outputs
  | FlowTee
  | FlowCross
  | FlowLambda (String,(Int,Int)) -- (scope,boxId)
  | FlowEvalLambda
  deriving Show

data FlowBox = FlowBox {
    flowBoxId :: (Int,Int),
    flowBoxType :: FlowBoxType,
    flowIn, flowUnknown :: Map Dir (Dir,(Int,Int)),
    flowOut :: Map Dir (Dir,Maybe (Int,Int))
    } deriving Show

resolveFlows :: [(Declaration,Map (Int,Int) Box)] -> [(Declaration,Map (Int,Int) FlowBox)]
resolveFlows declarations = map resolveGraphFlows declarations
  where
    signatures :: Map (Maybe String,String) Declaration
    signatures = Data.Map.fromList [(name decl,decl) | (decl,_) <- declarations, isFunction decl]
      where
        isFunction Declaration{declName=Nothing} = False
        isFunction _ = True
        name Declaration{declName=Just n,declScope=scope,declPrivate=private}
          | private = (Just scope,n)
          | otherwise = (Nothing,n)

    signature :: String -> String -> Declaration
    signature scope name =
        maybe (maybe (error ("Undefined function:" ++ name)) id (Data.Map.lookup (Nothing,name) signatures)) id (Data.Map.lookup (Just scope,name) signatures)

    callBoxType :: Declaration -> FlowBoxType
    callBoxType Declaration{declName=Just name,declScope=scope,declPrivate=private,declInputs=inputs,declOutputs=outputs} =
       FlowCall (if private then Just scope else Nothing,name) inputs outputs

    resolveGraphFlows :: (Declaration,Map (Int,Int) Box) -> (Declaration,Map (Int,Int) FlowBox)
    resolveGraphFlows (decl@Declaration{declScope=scope},boxes) = (decl,iterativeResolve (Data.Map.map (initialFlows scope) boxes))

    initialFlows :: String -> Box -> FlowBox
    initialFlows scope Box{boxType=boxType,boxId=boxId,boxContents=boxContents,boxN=n,boxS=s,boxE=e,boxW=w}
      | boxType == Box1 = FlowBox{flowBoxId=boxId,flowBoxType=FlowEvalLambda,flowIn=Data.Map.empty,flowOut=outs,flowUnknown=unknowns}
      | boxType == Box2Opp = FlowBox{flowBoxId=boxId,flowBoxType=FlowDecl,flowIn=Data.Map.empty,flowOut=Data.Map.union outs (Data.Map.map (fmap Just) unknowns),flowUnknown=Data.Map.empty}
      | boxType == Box2Adj = FlowBox{flowBoxId=boxId,flowBoxType=callBoxType (signature scope boxContents),flowIn=Data.Map.empty,flowOut=outs,flowUnknown=unknowns}
      | boxType == Box3 = FlowBox{flowBoxId=boxId,flowBoxType=FlowLambda (scope,boxId),flowIn=Data.Map.empty,flowOut=outs,flowUnknown=unknowns}
      | boxType == Box4 = FlowBox{flowBoxId=boxId,flowBoxType=FlowConst (if null boxContents then Nothing else Just (read (supportWeirdUnicodeMinus boxContents))),flowIn=Data.Map.empty,flowOut=Data.Map.union outs (Data.Map.map (fmap Just) unknowns),flowUnknown=Data.Map.empty}
      | boxType == Tee = FlowBox{flowBoxId=boxId,flowBoxType=FlowTee,flowIn=Data.Map.empty,flowOut=outs,flowUnknown=unknowns}
      | boxType == Cross = FlowBox{flowBoxId=boxId,flowBoxType=FlowCross,flowIn=Data.Map.empty,flowOut=outs,flowUnknown=unknowns}
      where
        outs = foldl addOut Data.Map.empty [(N,n),(S,s),(E,e),(W,w)]
        addOut outs (dir,BoxConnExit exitDir) = Data.Map.insert dir (exitDir,Nothing) outs
        addOut outs _ = outs
        unknowns = foldl addUnknown Data.Map.empty [(N,n),(S,s),(E,e),(W,w)]
        addUnknown unknowns (dir,BoxConn connDir connId) = Data.Map.insert dir (connDir,connId) unknowns
        addUnknown unknowns _ = unknowns

    iterativeResolve :: Map (Int,Int) FlowBox -> Map (Int,Int) FlowBox
    iterativeResolve = id --undefined

supportWeirdUnicodeMinus :: String -> String
supportWeirdUnicodeMinus str
  | take 1 str == "−" = '-' : drop 1 str
  | otherwise = str

data Subprogram = Subprogram {
    decl :: Declaration,
    outputs :: Map Dir Expr
    }

instance Show Subprogram where
    show Subprogram{decl=Declaration{declName=name,declScope=scope},outputs=outputs} = unlines (map showOutput (Data.Map.assocs outputs))
      where
        showOutput (dir,expr) = scope ++ maybe "" ("/" ++) name ++ ":" ++ show dir ++ "=" ++ show expr

data Expr =
    ExprConst (Maybe Integer)
  | ExprInput Dir
  | ExprCall Subprogram [(Dir,Expr)] Dir
  | ExprNand Expr Expr
  | ExprLessThan Expr Expr
  | ExprShiftLeft Expr Expr
    -- Since lambdas are not closures, they can be statically enumerated.
  | ExprLambda Integer
  | ExprEvalLambda1 Expr Expr
  | ExprEvalLambda2 Expr Expr

instance Show Expr where
    show (ExprConst Nothing) = "stdin"
    show (ExprConst (Just n)) = show n
    show (ExprInput dir) = show dir
    show (ExprCall Subprogram{decl=Declaration{declName=Just name,declScope=scope}} args dir) = "call(" ++ show scope ++ "/" ++ show name ++ ":" ++ show args ++ show dir ++ ")"
    show (ExprNand a b) = "nand(" ++ show a ++ "," ++ show b ++ ")"
    show (ExprLessThan a b) = "lt(" ++ show a ++ "," ++ show b ++ ")"
    show (ExprShiftLeft a b) = "shl(" ++ show a ++ "," ++ show b ++ ")"
    show (ExprLambda n) = "lambda(" ++ show n ++ ")"
    show (ExprEvalLambda1 lambda inp) = "evallambda1(" ++ show lambda ++ "," ++ show inp ++ ")"
    show (ExprEvalLambda2 lambda inp) = "evallambda2(" ++ show lambda ++ "," ++ show inp ++ ")"

resolve :: [(Declaration,Map (Int,Int) Box)] -> (Map Integer (Expr,Expr),[Subprogram])
resolve declarations = foldl resolve1 (Data.Map.empty,[]) declarations
  where resolve1 = undefined

parse :: [(String,String)] -> (Map Integer (Expr,Expr),[Expr])
parse prog
  | null errs = fmap (concatMap findOutputs) (resolve declarations)
  | otherwise = error (unlines errs)
  where
    declarations = concatMap parseDeclarations prog
    errs = declarationErrors (map fst declarations)
    findOutputs Subprogram{decl=Declaration{declName=Nothing},outputs=outputs} = Data.Map.elems outputs
    findOutputs _ = []

eval :: (Integer,Map Integer (Expr,Expr)) -> [(Dir,Integer)] -> Expr -> Integer
eval (stdin,lambdas) inputs (ExprConst Nothing) = stdin
eval (stdin,lambdas) inputs (ExprConst (Just n)) = n
eval (stdin,lambdas) inputs (ExprInput dir) = n
  where Just n = lookup dir inputs
eval (stdin,lambdas) inputs (ExprCall Subprogram{outputs=outputs} params dir) =
    eval (stdin,lambdas) callInputs (outputs Data.Map.! dir)
  where callInputs = map (fmap (eval (stdin,lambdas) inputs)) params
eval (stdin,lambdas) inputs (ExprNand a b) = complement (ev a .&. ev b)
  where ev = eval (stdin,lambdas) inputs
eval (stdin,lambdas) inputs (ExprLessThan a b) = if ev a < ev b then -1 else 0
  where ev = eval (stdin,lambdas) inputs
eval (stdin,lambdas) inputs (ExprShiftLeft a b) = shift (ev a) (fromIntegral (ev b))
  where ev = eval (stdin,lambdas) inputs
eval (stdin,lambdas) inputs (ExprLambda n) = n
eval (stdin,lambdas) inputs (ExprEvalLambda1 lambda input) = evalLambda (stdin,lambdas) inputs lambda input fst
eval (stdin,lambdas) inputs (ExprEvalLambda2 lambda input) = evalLambda (stdin,lambdas) inputs lambda input snd

evalLambda :: (Integer,Map Integer (Expr,Expr)) -> [(Dir,Integer)] -> Expr -> Expr -> ((Expr,Expr) -> Expr) -> Integer
evalLambda (stdin,lambdas) inputs lambda input chooseOutput =
    eval (stdin,lambdas) (zip [N,S,E,W] (repeat (ev input))) (chooseOutput (lambdas Data.Map.! (ev lambda)))
  where ev = eval (stdin,lambdas) inputs

run :: [(String,String)] -> Integer -> [Integer]
run prog inp = map (eval (inp,lambdas) []) outputs
  where (lambdas,outputs) = parse prog

encodeString :: String -> Integer
encodeString str = enc 1 0 str
  where
    enc f n "" = n - f
    enc f n (c:cs) = enc (f*2097152) (n + f*fromIntegral (ord c)) cs

decodeString :: Integer -> String
decodeString n
  | n == 0 || n == -1 = ""
  | otherwise = chr (fromIntegral (n `mod` 2097152)) : decodeString (n `div` 2097152)

funkytown :: String -> IO ()
funkytown prog = interact (decodeString . head . run [("",prog)] . encodeString)

main :: IO ()
main = do
    srcFiles <- getArgs
    srcs <- mapM readFile srcFiles
    interact (decodeString . head . run (zip srcFiles srcs) . encodeString)
