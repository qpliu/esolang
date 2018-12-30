-- https://esolangs.org/wiki/Funciton

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
    grid = array ((1,1),(width,height)) (concat (zipWith makeGridLine [1..] (lines (stripWeirdUnicodeJunk src))))
      where
        width = maximum (map length (lines src))
        height = length (lines src)
        makeGridLine y line = zip (map (flip (,) y) [1..]) (take width (line ++ repeat ' '))

    stripWeirdUnicodeJunk :: String -> String
    stripWeirdUnicodeJunk src
      | take 1 src == "\65279" = drop 1 src
      | otherwise = src

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
    } deriving (Eq,Ord,Show)

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
  | FlowCall Declaration
  | FlowTee
  | FlowCross
  | FlowLambda
  | FlowInvokeLambda
  deriving (Eq,Show)

data FlowBox = FlowBox {
    flowBoxId :: (Int,Int),
    flowBoxType :: FlowBoxType,
    flowIn, flowUnknown :: Map Dir (Dir,(Int,Int)),
    flowOut :: Map Dir (Dir,Maybe (Int,Int))
    } deriving Show

flowValidCallRotations :: FlowBox -> [Int]
flowValidCallRotations FlowBox{flowBoxType=FlowCall Declaration{declInputs=callIns,declOutputs=callOuts},flowUnknown=unknowns,flowIn=ins,flowOut=outs} = filter isValid [0..3]
  where
    unknownDirs rot = map (rotate rot) (Data.Map.keys unknowns)
    inDirs rot = map (rotate rot) (Data.Map.keys ins)
    outDirs rot = map (rotate rot) (Data.Map.keys outs)
    isValid rot 
      | any (`Data.Set.member` callOuts) (inDirs rot) = False
      | any (`Data.Set.member` callIns) (outDirs rot) = False
      | any (not . (`Data.Set.member` callIns)) (inDirs rot) = False
      | any (not . (`Data.Set.member` callOuts)) (outDirs rot) = False
      | any (not . (`Data.Set.member` (Data.Set.union callIns callOuts))) (unknownDirs rot ++ inDirs rot ++ outDirs rot) = False
      | otherwise = True

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

    resolveGraphFlows :: (Declaration,Map (Int,Int) Box) -> (Declaration,Map (Int,Int) FlowBox)
    resolveGraphFlows (decl@Declaration{declScope=scope},boxes) = (decl,iterativeResolve (Data.Map.map (initialFlows scope) boxes))

    initialFlows :: String -> Box -> FlowBox
    initialFlows scope Box{boxType=boxType,boxId=boxId,boxContents=boxContents,boxN=n,boxS=s,boxE=e,boxW=w}
      | boxType == Box1 && Data.Map.size outs > 2 = error ("Lambda invocation with more than 2 outputs:"++show boxId)
      | boxType == Box1 && Data.Map.size outs + Data.Map.size unknowns /= 4 = error ("Lambda invocation without 4 connections:"++show boxId)
      | boxType == Box1 = FlowBox{flowBoxId=boxId,flowBoxType=FlowInvokeLambda,flowIn=Data.Map.empty,flowOut=outs,flowUnknown=unknowns}
      | boxType == Box2Opp = FlowBox{flowBoxId=boxId,flowBoxType=FlowDecl,flowIn=Data.Map.empty,flowOut=Data.Map.union outs (Data.Map.map (fmap Just) unknowns),flowUnknown=Data.Map.empty}
      | boxType == Box2Adj = FlowBox{flowBoxId=boxId,flowBoxType=FlowCall (signature scope boxContents),flowIn=Data.Map.empty,flowOut=outs,flowUnknown=unknowns}
      | boxType == Box3 && Data.Map.size outs > 2 = error ("Lambda creation with more than 2 outputs:"++show boxId)
      | boxType == Box3 && Data.Map.size outs + Data.Map.size unknowns /= 4 = error ("Lambda creation without 4 connections:"++show boxId)
      | boxType == Box3 = FlowBox{flowBoxId=boxId,flowBoxType=FlowLambda,flowIn=Data.Map.empty,flowOut=outs,flowUnknown=unknowns}
      | boxType == Box4 = FlowBox{flowBoxId=boxId,flowBoxType=FlowConst (if null boxContents then Nothing else Just (read (supportWeirdUnicodeMinus boxContents))),flowIn=Data.Map.empty,flowOut=Data.Map.union outs (Data.Map.map (fmap Just) unknowns),flowUnknown=Data.Map.empty}
      | boxType == Tee && Data.Map.size outs == 2 && Data.Set.fromList (Data.Map.keys outs) /= Data.Set.fromList (map (rotate 2) (Data.Map.keys outs)) = error ("T with adjacent outputs:"++show boxId)
      | boxType == Tee = FlowBox{flowBoxId=boxId,flowBoxType=FlowTee,flowIn=Data.Map.empty,flowOut=outs,flowUnknown=unknowns}
      | boxType == Cross && Data.Map.size outs > 2 = error ("Cross with more than 2 outputs:"++show boxId)
      | boxType == Cross && Data.Map.size outs == 2 && Data.Set.fromList (Data.Map.keys outs) == Data.Set.fromList (map (rotate 2) (Data.Map.keys outs)) = error ("Cross with opposing outputs:"++show boxId)
      | boxType == Cross = FlowBox{flowBoxId=boxId,flowBoxType=FlowCross,flowIn=Data.Map.empty,flowOut=outs,flowUnknown=unknowns}
      where
        outs = foldl addOut Data.Map.empty [(N,n),(S,s),(E,e),(W,w)]
        addOut outs (dir,BoxConnExit exitDir) = Data.Map.insert dir (exitDir,Nothing) outs
        addOut outs _ = outs
        unknowns = foldl addUnknown Data.Map.empty [(N,n),(S,s),(E,e),(W,w)]
        addUnknown unknowns (dir,BoxConn connDir connId) = Data.Map.insert dir (connDir,connId) unknowns
        addUnknown unknowns _ = unknowns

    iterativeResolve :: Map (Int,Int) FlowBox -> Map (Int,Int) FlowBox
    iterativeResolve flowBoxes = resolve1 (Data.Map.elems flowBoxes)
      where
        resolve1 [] = flowBoxes
        resolve1 (box@FlowBox{flowBoxId=boxId,flowBoxType=boxType,flowUnknown=unknowns}:boxes)
          | Data.Map.null unknowns = resolve1 boxes
          -- if other end of connection is determined, connection is determined
          | otherEndDetermined (Data.Map.elems unknowns) = iterativeResolve (Data.Map.insert boxId (updateFromOtherEnd box (head (Data.Map.assocs unknowns))) flowBoxes)
          | otherEndDetermined (drop 1 (Data.Map.elems unknowns)) = iterativeResolve (Data.Map.insert boxId (updateFromOtherEnd box (head (drop 1 (Data.Map.assocs unknowns)))) flowBoxes)
          | otherEndDetermined (drop 2 (Data.Map.elems unknowns)) = iterativeResolve (Data.Map.insert boxId (updateFromOtherEnd box (head (drop 2 (Data.Map.assocs unknowns)))) flowBoxes)
          | otherEndDetermined (drop 3 (Data.Map.elems unknowns)) = iterativeResolve (Data.Map.insert boxId (updateFromOtherEnd box (head (drop 3 (Data.Map.assocs unknowns)))) flowBoxes)
          -- any connection determines the other 2 connections for Tee
          | boxType == FlowTee && Data.Map.size unknowns < 3 = iterativeResolve (Data.Map.insert boxId (determineTeeFlow box) flowBoxes)
          -- call with 1 valid rotation is determined
          | flowIsCall box && length (flowValidCallRotations box) == 1 = iterativeResolve (Data.Map.insert boxId (determineCall box (head (flowValidCallRotations box))) flowBoxes)
          -- inputs and outputs are always on opposite sides of cross, lambda creations, and lambda invocations
          | flowIsCross box && oppositeSideDetermined box (Data.Map.keys unknowns) = iterativeResolve (Data.Map.insert boxId (updateFromOppositeSide box (head (Data.Map.keys unknowns))) flowBoxes)
          | flowIsCross box && oppositeSideDetermined box (drop 1 (Data.Map.keys unknowns)) = iterativeResolve (Data.Map.insert boxId (updateFromOppositeSide box (head (drop 1 (Data.Map.keys unknowns)))) flowBoxes)
          | flowIsCross box && oppositeSideDetermined box (drop 2 (Data.Map.keys unknowns)) = iterativeResolve (Data.Map.insert boxId (updateFromOppositeSide box (head (drop 2 (Data.Map.keys unknowns)))) flowBoxes)
          -- Starkov construct is a sink (could syntactically be a source, but using it as such ought to be an error)
          | boxType == FlowTee && flowIsStarkov box = iterativeResolve (Data.Map.insert boxId (determineStarkov box) flowBoxes)
          | otherwise = resolve1 boxes

        otherEndDetermined :: [(Dir,(Int,Int))] -> Bool
        otherEndDetermined ((otherDir,otherId):_) = Data.Map.notMember otherDir (flowUnknown (flowBoxes Data.Map.! otherId))
        otherEndDetermined [] = False
        updateFromOtherEnd :: FlowBox -> (Dir,(Dir,(Int,Int))) -> FlowBox
        updateFromOtherEnd box@FlowBox{flowUnknown=unknowns,flowIn=ins,flowOut=outs} (dir,(otherDir,otherId))
          | Data.Map.member otherDir (flowIn (flowBoxes Data.Map.! otherId)) = box{flowUnknown=Data.Map.delete dir unknowns,flowOut=Data.Map.insert dir (otherDir,Just otherId) outs}
          | Data.Map.member otherDir (flowOut (flowBoxes Data.Map.! otherId)) = box{flowUnknown=Data.Map.delete dir unknowns,flowIn=Data.Map.insert dir (otherDir,otherId) ins}
          | otherwise = error ("wtf@"++show dir++","++show (flowBoxId box))

        determineTeeFlow box@FlowBox{flowUnknown=unknowns,flowIn=ins,flowOut=outs}
          | Data.Map.member (rotate 2 dir) ins = box{flowUnknown=Data.Map.delete dir unknowns,flowIn=Data.Map.insert dir dest ins}
          | Data.Map.member (rotate 1 dir) ins = box{flowUnknown=Data.Map.delete dir unknowns,flowOut=Data.Map.insert dir (fmap Just dest) outs}
          | Data.Map.member (rotate 3 dir) ins = box{flowUnknown=Data.Map.delete dir unknowns,flowOut=Data.Map.insert dir (fmap Just dest) outs}
          | Data.Map.member (rotate 2 dir) outs = box{flowUnknown=Data.Map.delete dir unknowns,flowOut=Data.Map.insert dir (fmap Just dest) outs}
          | Data.Map.member (rotate 1 dir) outs = box{flowUnknown=Data.Map.delete dir unknowns,flowIn=Data.Map.insert dir dest ins}
          | Data.Map.member (rotate 3 dir) outs = box{flowUnknown=Data.Map.delete dir unknowns,flowIn=Data.Map.insert dir dest ins}
          | otherwise = error ("wtf@"++show box)
          where (dir,dest) = head (Data.Map.assocs unknowns)

        flowIsCall :: FlowBox -> Bool
        flowIsCall FlowBox{flowBoxType=FlowCall _} = True
        flowIsCall _ = False
        determineCall :: FlowBox -> Int -> FlowBox
        determineCall box@FlowBox{flowBoxType=FlowCall Declaration{declInputs=callIns,declOutputs=callOuts},flowUnknown=unknowns,flowIn=ins,flowOut=outs} rot
          | Data.Set.member (rotate rot dir) callIns = box{flowUnknown=Data.Map.delete dir unknowns,flowIn=Data.Map.insert dir dest ins}
          | Data.Set.member (rotate rot dir) callOuts = box{flowUnknown=Data.Map.delete dir unknowns,flowOut=Data.Map.insert dir (fmap Just dest) outs}
          | otherwise = error ("wtf@"++show box)
          where ((dir,dest):_) = Data.Map.assocs unknowns

        flowIsCross :: FlowBox -> Bool
        flowIsCross FlowBox{flowBoxType=FlowCross} = True
        flowIsCross FlowBox{flowBoxType=FlowLambda} = True
        flowIsCross FlowBox{flowBoxType=FlowInvokeLambda} = True
        flowIsCross _ = False
        oppositeSideDetermined :: FlowBox -> [Dir] -> Bool
        oppositeSideDetermined _ [] = False
        oppositeSideDetermined FlowBox{flowIn=ins,flowOut=outs} (dir:_) =
            Data.Map.member (rotate 2 dir) ins || Data.Map.member (rotate 2 dir) outs
        updateFromOppositeSide :: FlowBox -> Dir -> FlowBox
        updateFromOppositeSide box@FlowBox{flowUnknown=unknowns,flowIn=ins,flowOut=outs} dir
          | Data.Map.member (rotate 2 dir) ins = box{flowUnknown=Data.Map.delete dir unknowns,flowOut=Data.Map.insert dir (fmap Just (unknowns Data.Map.! dir)) outs}
          | Data.Map.member (rotate 2 dir) outs = box{flowUnknown=Data.Map.delete dir unknowns,flowIn=Data.Map.insert dir (unknowns Data.Map.! dir) ins}
          | otherwise = error ("wtf@"++show box)

        flowIsStarkov :: FlowBox -> Bool
        flowIsStarkov FlowBox{flowBoxId=boxId,flowUnknown=unknowns} =
            not (Data.Map.null (Data.Map.filter ((== boxId) . snd) unknowns))
        determineStarkov :: FlowBox -> FlowBox
        determineStarkov box@FlowBox{flowBoxId=boxId,flowUnknown=unknowns} = box{flowUnknown=Data.Map.delete dir unknowns,flowIn=Data.Map.fromList [(dir,dest)]}
          where [(dir,dest)] = filter ((/= boxId) . snd . snd) (Data.Map.assocs unknowns)

    supportWeirdUnicodeMinus :: String -> String
    supportWeirdUnicodeMinus str
      | take 1 str == "−" = '-' : drop 1 str
      | otherwise = str

flowErrors :: (Declaration,Map (Int,Int) FlowBox) -> [String]
flowErrors (decl@Declaration{declScope=scope},flows) = concat [concatMap unknownConnections boxes,concatMap connectionMismatches boxes,concatMap invalidOrientations boxes,concatMap ambiguousOrInvalidCalls boxes]
  where
    boxes :: [FlowBox]
    boxes = Data.Map.elems flows

    unknownConnections :: FlowBox -> [String]
    unknownConnections FlowBox{flowBoxId=boxId,flowUnknown=unknown}
      | Data.Map.null unknown = []
      | otherwise = ["Unresolvable connection "++scope++show boxId++show (Data.Map.keys unknown)]

    -- input not connected to output or output not connected to input
    connectionMismatches :: FlowBox -> [String]
    connectionMismatches FlowBox{flowBoxId=boxId,flowIn=ins,flowOut=outs} =
        Data.Map.fold checkFlowIn (Data.Map.fold checkFlowOut [] outs) ins
      where
        checkFlowIn (dir,srcBoxId) errs
          | Data.Map.member dir (flowOut (flows Data.Map.! srcBoxId)) = errs
          | otherwise = ("Mismatch flow to "++scope++show boxId):errs
        checkFlowOut (dir,Nothing) errs = errs
        checkFlowOut (dir,Just destBoxId) errs
          | Data.Map.member dir (flowIn (flows Data.Map.! destBoxId)) = errs
          | otherwise = ("Mismatch flow from "++scope++show boxId):errs

    -- invalid tee/cross/lambda creation/lambda invocation inputs/outputs orientations
    invalidOrientations :: FlowBox -> [String]
    invalidOrientations FlowBox{flowBoxId=boxId,flowBoxType=FlowTee,flowIn=ins,flowOut=outs}
      | Data.Map.size ins == 1 && Data.Map.member (rotate 1 inDir) outs && Data.Map.member (rotate 3 inDir) outs = []
      | Data.Map.size outs == 1 && Data.Map.member (rotate 1 outDir) ins && Data.Map.member (rotate 3 outDir) ins = []
      | otherwise = ["Invalid flows through "++scope++show boxId]
      where
        inDir = head (Data.Map.keys ins)
        outDir = head (Data.Map.keys outs)
    invalidOrientations FlowBox{flowBoxId=boxId,flowBoxType=FlowCross,flowIn=ins,flowOut=outs} = invalid4Orientation boxId ins outs
    invalidOrientations FlowBox{flowBoxId=boxId,flowBoxType=FlowLambda,flowIn=ins,flowOut=outs} = invalid4Orientation boxId ins outs
    invalidOrientations FlowBox{flowBoxId=boxId,flowBoxType=FlowInvokeLambda,flowIn=ins,flowOut=outs} = invalid4Orientation boxId ins outs
    invalidOrientations _ = []
    invalid4Orientation :: (Int,Int) -> (Map Dir ((Dir,(Int,Int)))) -> (Map Dir ((Dir,Maybe (Int,Int)))) -> [String]
    invalid4Orientation boxId ins outs
      | Data.Map.size ins == 2 && all ((`Data.Map.member` outs) . rotate 2) (Data.Map.keys ins) = []
      | otherwise = ["Invalid flows through "++scope++show boxId]

    -- not exactly one valid rotation for calls
    ambiguousOrInvalidCalls :: FlowBox -> [String]
    ambiguousOrInvalidCalls box@FlowBox{flowBoxId=boxId,flowBoxType=FlowCall _}
      | length (flowValidCallRotations box) == 1 = []
      | otherwise = ["Ambiguous or invalid function call "++scope++show boxId]
    ambiguousOrInvalidCalls _ = []

type ElementId = [(String,(Int,Int))]
type ExprId = (Dir,ElementId)

data Subprogram = Subprogram {
    decl :: Declaration,
    outputs :: Map Dir ExprId,
    exprs :: Map ExprId Expr
    }

instance Show Subprogram where
    show Subprogram{decl=Declaration{declName=name,declScope=scope,declInputs=inputs},outputs=outputs,exprs=exprs} = unlines ([maybe ("prog " ++ scope) ((++ show (Data.Set.elems inputs)) . ("fn " ++)) name ++ " {"] ++ map showItem (Data.Map.assocs outputs) ++ map showItem (Data.Map.assocs exprs) ++ ["}"])
      where showItem (a,b) = "  " ++ show a ++ "=" ++ show b

data Expr =
    ExprConst (Maybe Integer)
  | ExprInput Dir
  | ExprCall ElementId Declaration (Map Dir ExprId) Dir Int
  | ExprNand ElementId ExprId ExprId
  | ExprExpr ElementId ExprId
  | ExprLessThan ElementId ExprId ExprId
  | ExprShiftLeft ElementId ExprId ExprId
  | ExprLambda ElementId ExprId ExprId
  | ExprLambdaInput ElementId
  | ExprInvokeLambda1 ElementId ExprId ExprId
  | ExprInvokeLambda2 ElementId ExprId ExprId

instance Show Expr where
    show (ExprConst Nothing) = "stdin"
    show (ExprConst (Just n)) = show n
    show (ExprInput dir) = show dir
    show (ExprCall _ Declaration{declName=Just name} args dir rot) = "call " ++ show dir ++ show rot ++ " " ++ name ++ show (Data.Map.assocs args)
    show (ExprNand _ a b) = "nand(" ++ show a ++ "," ++ show b ++ ")"
    show (ExprExpr _ a) = "expr(" ++ show a ++ ")"
    show (ExprLessThan _ a b) = "lt(" ++ show a ++ "," ++ show b ++ ")"
    show (ExprShiftLeft _ a b) = "shl(" ++ show a ++ "," ++ show b ++ ")"
    show (ExprLambda lambdaId out1 out2) = "lambda" ++ show lambdaId ++ "(out1=" ++ show out1 ++ ",out2=" ++ show out2 ++ ")"
    show (ExprLambdaInput lambdaId) = "lambdainput" ++ show lambdaId
    show (ExprInvokeLambda1 _ lambda inp) = "invokelambda1(" ++ show lambda ++ "," ++ show inp ++ ")"
    show (ExprInvokeLambda2 _ lambda inp) = "invokelambda2(" ++ show lambda ++ "," ++ show inp ++ ")"

resolve :: (Declaration,Map (Int,Int) FlowBox) -> Subprogram
resolve (decl@Declaration{declScope=scope},flowBoxes) = Subprogram{decl=decl,outputs=Data.Map.fromList subprogramOutputs,exprs=exprs}
  where
    subprogramName :: (Maybe String,String)
    subprogramName = (if declPrivate decl then Just scope else Nothing,maybe "" id (declName decl))

    exprs :: Map ExprId Expr
    exprs = Data.Map.fromList (concatMap makeBoxOutputExprs (Data.Map.elems flowBoxes))

    makeBoxOutputExprs :: FlowBox -> [(ExprId,Expr)]
    makeBoxOutputExprs FlowBox{flowBoxId=boxId,flowBoxType=FlowDecl,flowOut=outs} = [((dir,[(scope,boxId)]),ExprInput (rotate 2 dir)) | dir <- Data.Map.keys outs]
    makeBoxOutputExprs FlowBox{flowBoxId=boxId,flowBoxType=FlowConst c,flowOut=outs} = [((dir,[(scope,boxId)]),ExprConst c) | dir <- Data.Map.keys outs]
    makeBoxOutputExprs box@FlowBox{flowBoxId=boxId,flowBoxType=FlowCall callee,flowIn=ins,flowOut=outs} = [((dir,[(scope,boxId)]),ExprCall [(scope,boxId)] callee (Data.Map.fromList (map makeParam (Data.Map.assocs ins))) (rotate rot dir) rot) | dir <- Data.Map.keys outs]
      where
        rot = head (flowValidCallRotations box)
        makeParam (argDir,(argFromDir,boxId)) = (rotate rot argDir,(argFromDir,[(scope,boxId)]))
    makeBoxOutputExprs FlowBox{flowBoxId=boxId,flowBoxType=FlowTee,flowIn=ins,flowOut=outs}
      | Data.Map.size outs == 2 = [((dir,[(scope,boxId)]),ExprExpr [(scope,boxId)] (fmap ((:[]) . (,) scope) (head (Data.Map.elems ins)))) | dir <- Data.Map.keys outs]
      | otherwise = [((outDir,[(scope,boxId)]),ExprNand [(scope,boxId)] (fmap ((:[]) . (,) scope) (ins Data.Map.! rotate 3 outDir)) (fmap ((:[]) . (,) scope) (ins Data.Map.! rotate 1 outDir)))]
      where outDir = head (Data.Map.keys outs)
    makeBoxOutputExprs box@FlowBox{flowBoxId=boxId,flowBoxType=FlowCross} =
        [((output1Dir,[(scope,boxId)]),ExprShiftLeft [(scope,boxId)] input1 input2),((output2Dir,[(scope,boxId)]),ExprLessThan [(scope,boxId)] input1 input2)]
      where (input1,input2,output1Dir,output2Dir) = box4Connections box
    makeBoxOutputExprs box@FlowBox{flowBoxId=boxId,flowBoxType=FlowLambda} =
        [((output1Dir,[(scope,boxId)]),ExprLambda [(scope,boxId)] input1 input2),((output2Dir,[(scope,boxId)]),ExprLambdaInput [(scope,boxId)])]
      where (input1,input2,output1Dir,output2Dir) = box4Connections box
    makeBoxOutputExprs box@FlowBox{flowBoxId=boxId,flowBoxType=FlowInvokeLambda} =
        [((output1Dir,[(scope,boxId)]),ExprInvokeLambda1 [(scope,boxId)] input1 input2),((output2Dir,[(scope,boxId)]),ExprInvokeLambda2 [(scope,boxId)] input1 input2)]
      where (input1,input2,output1Dir,output2Dir) = box4Connections box

    subprogramOutputs :: [(Dir,ExprId)]
    subprogramOutputs = Data.Map.fold findSubprogramOutputs [] flowBoxes
      where
        findSubprogramOutputs FlowBox{flowBoxId=boxId,flowOut=outs} outputs =
            Data.Map.foldWithKey (findSubprogramOutput boxId) outputs outs
        findSubprogramOutput boxId flowDir (outputDir,Nothing) outputs = (outputDir,(flowDir,[(scope,boxId)])) : outputs
        findSubprogramOutput _ _ _ outputs = outputs

    box4Connections :: FlowBox -> (ExprId,ExprId,Dir,Dir) -- (input1,input2,output1Dir,output2Dir)
    box4Connections box@FlowBox{flowIn=ins}
      | all (`Data.Map.member` ins) [N,W] = (getInExprId N,getInExprId W,S,E)
      | all (`Data.Map.member` ins) [W,S] = (getInExprId W,getInExprId S,E,N)
      | all (`Data.Map.member` ins) [S,E] = (getInExprId S,getInExprId E,N,W)
      | all (`Data.Map.member` ins) [E,N] = (getInExprId E,getInExprId N,W,S)
      | otherwise = error ("wtf@"++show box)
      where getInExprId dir = fmap ((:[]) . (,) scope) (ins Data.Map.! dir)

type Program = Map Declaration Subprogram

removeUnreachableFunctions :: Program -> Program
removeUnreachableFunctions program = program

inliner :: Program -> Program
inliner program = program
-- undefined iteratively inline subprograms that do not call any subprograms

optimizer :: Program -> Program
optimizer program = program
-- undefined remove ExprExpr exprs from splitters and inlined cross-NOP calls

parse :: [(String,String)] -> ([Expr],Program)
parse prog
  | not (null declErrs) = error (unlines declErrs)
  | not (null flowErrs) = error (unlines flowErrs)
  | otherwise = (concatMap findOutputs (Data.Map.elems program),program)
  where
    declarations = concatMap parseDeclarations prog
    declErrs = declarationErrors (map fst declarations)
    flows = resolveFlows declarations
    flowErrs = concatMap flowErrors flows
    subprograms = map resolve flows
    program = inliner (Data.Map.fromList [(decl,subprogram) | subprogram@Subprogram{decl=decl} <- subprograms])
    findOutputs Subprogram{decl=Declaration{declName=Nothing},outputs=outputs,exprs=exprs} = map (exprs Data.Map.!) (Data.Map.elems outputs)
    findOutputs _ = []

{-
data State = State {
    stStdin :: Integer,
    stNextLambda :: Integer,
    stCallArgs :: Map Dir Value,
    stLambdaArgs :: Map LambdaId Value,
    stExprMemos :: Map ExprId (Map Dir Value)
    }

data Value = Value Integer (Map Integer Closure)
type Closure = (LambdaId,((Expr,Expr),State))

eval :: State -> Expr -> (State,Value)
eval state (ExprConst Nothing) = (state,Value (stStdin state) Data.Map.empty)
eval state (ExprConst (Just n)) = (state,Value n Data.Map.empty)
eval state (ExprInput dir) = (state,stCallArgs state Data.Map.! dir)
eval state@State{stExprMemos=memos} (ExprCall exprId Subprogram{outputs=outputs} params dir _)
  | exprId `Data.Map.member` memos = (state,(memos Data.Map.! exprId) Data.Map.! dir)
  | otherwise = (callerState{stNextLambda=stNextLambda returnState,stExprMemos=Data.Map.insert exprId returnValues (stExprMemos callerState)},returnValues Data.Map.! dir)
  where
    (callerState,callArgs) = foldl evalCall (state,[]) params
    (returnState,returnVals) = foldl evalCall (callerState{stCallArgs=Data.Map.fromList callArgs,stLambdaArgs=Data.Map.empty,stExprMemos=Data.Map.empty},[]) (Data.Map.assocs outputs)
    evalCall (state,values) (dir,expr) = (nextState,(dir,val):values)
      where (nextState,val) = eval state expr
    returnValues = Data.Map.fromList returnVals
eval state (ExprNand expr1 expr2)
  | val1 == 0 = (state1,Value (-1) closures1)
  | otherwise = (state2,Value (complement (val1 .&. val2)) (Data.Map.union closures1 closures2))
  where
    (state1,Value val1 closures1) = eval state expr1
    (state2,Value val2 closures2) = eval state1 expr2
eval state (ExprSplitter exprId expr) = maybe evalSplit ((,) state . (Data.Map.! N)) (Data.Map.lookup exprId (stExprMemos state))
  where
    evalSplit = (state1{stExprMemos=Data.Map.insert exprId (Data.Map.fromList [(N,value1)]) (stExprMemos state1)},value1)
    (state1,value1) = eval state expr
eval state@State{stExprMemos=memos} (ExprLessThan exprId expr1 expr2)
  | exprId `Data.Map.member` memos = (state,evalLt ((memos Data.Map.! exprId) Data.Map.! N) ((memos Data.Map.! exprId) Data.Map.! W))
  | otherwise = (state2{stExprMemos=Data.Map.insert exprId (Data.Map.fromList [(N,value1),(W,value2)]) (stExprMemos state2)},evalLt value1 value2)
  where
    evalLt (Value val1 _) (Value val2 _) = Value (if val1 < val2 then -1 else 0) Data.Map.empty
    (state1,value1) = eval state expr1
    (state2,value2) = eval state1 expr2
eval state@State{stExprMemos=memos} (ExprShiftLeft exprId expr1 expr2)
  | exprId `Data.Map.member` memos = (state,evalShift ((memos Data.Map.! exprId) Data.Map.! N) ((memos Data.Map.! exprId) Data.Map.! W))
  | otherwise = (state2{stExprMemos=Data.Map.insert exprId (Data.Map.fromList [(N,value1),(W,value2)]) (stExprMemos state2)},evalShift value1 value2)
  where
    evalShift (Value val1 closures1) (Value val2 closures2) = Value (shiftInteger val1 val2) (Data.Map.union closures1 closures2)
    (state1,value1) = eval state expr1
    (state2,value2) = eval state1 expr2
eval state (ExprLambda lambdaId expr1 expr2) = (state{stNextLambda=1+stNextLambda state},Value (stNextLambda state) (Data.Map.fromList [(stNextLambda state,(lambdaId,((expr1,expr2),state)))]))
eval state (ExprInvokeLambda1 exprId lambda expr) = evalInvokeLambda exprId state lambda expr fst
eval state (ExprInvokeLambda2 exprId lambda expr) = evalInvokeLambda exprId state lambda expr snd

shiftInteger :: Integer -> Integer -> Integer
shiftInteger a b
  | b > fromIntegral (maxBound :: Int) = shiftInteger (shift a maxBound) (b - fromIntegral (maxBound :: Int))
  | b < fromIntegral (minBound :: Int) = shiftInteger (shift a minBound) (b - fromIntegral (minBound :: Int))
  | otherwise = shift a (fromIntegral b)

evalInvokeLambda :: ExprId -> State -> Expr -> Expr -> ((Value,Value) -> Value) -> (State,Value)
evalInvokeLambda exprId state@State{stExprMemos=memos} lambdaExpr inputExpr chooseOutput-- = (state2{stNextLambda=stNextLambda state3},value3)
  | exprId `Data.Map.member` memos = (state,chooseOutput ((memos Data.Map.! exprId) Data.Map.! S,(memos Data.Map.! exprId) Data.Map.! E))
  | otherwise = (state2{stNextLambda=stNextLambda state4,stExprMemos=Data.Map.insert exprId (Data.Map.fromList [(S,output1Value),(E,output2Value)]) (stExprMemos state2)},chooseOutput (output1Value,output2Value))
  where
    (state1,Value lambdaValue lambdaClosures) = eval state lambdaExpr
    (state2,input) = eval state1 inputExpr
    (lambdaId,((output1Expr,output2Expr),lambdaState)) = lambdaClosures Data.Map.! lambdaValue
    (state3,output1Value) = eval lambdaState{stNextLambda=stNextLambda state2,stLambdaArgs=Data.Map.insert lambdaId input (stLambdaArgs lambdaState)} output1Expr
    (state4,output2Value) = eval state3 output2Expr

run :: [(String,String)] -> Integer -> [Integer]
run prog inp = map (getVal . eval State{stStdin=inp,stNextLambda=1,stCallArgs=Data.Map.empty,stLambdaArgs=Data.Map.empty,stExprMemos=Data.Map.empty}) (parse prog)
  where getVal (_,(Value val _)) = val

encodeString :: String -> Integer
encodeString str = enc 1 0 str
  where
    enc f n "" = n - f
    enc f n (c:cs) = enc (f*2097152) (n + f*fromIntegral (ord c)) cs

decodeString :: Integer -> String
decodeString n
  | n == 0 || n == -1 = ""
  | otherwise = chr (fromIntegral (n `mod` 2097152)) : decodeString (n `div` 2097152)

funky :: [String] -> Integer -> IO [Integer]
funky srcFiles input = do
    srcs <- mapM readFile srcFiles
    return (run (zip srcFiles srcs) input)

funkytown :: [String] -> String -> IO [String]
funkytown srcFiles input = do
    srcs <- mapM readFile srcFiles
    return (map decodeString (run (zip srcFiles srcs) (encodeString input)))

funciton :: String -> IO ()
funciton prog = interact (decodeString . head . run [("",prog)] . encodeString)

main :: IO ()
main = do
    srcFiles <- getArgs
    srcs <- mapM readFile srcFiles
    interact (decodeString . head . run (zip srcFiles srcs) . encodeString)
-}

tmp srcFiles = do
    srcs <- mapM readFile srcFiles
    let flows = resolveFlows (concatMap parseDeclarations (zip srcFiles srcs))
    let subprograms = map resolve flows
    mapM_ print subprograms

-- Example of an ambiguous function call:
--                        ┌──────┐
-- ╓───╖ ┌───╖   ╔═══╗  ┌─┴─╖  ┌─┴─╖
-- ║ @ ╟─┤ f ╟─  ║   ╟──┤ @ ╟─ │ f ║
-- ╙─┬─╜ ╘═══╝   ╚═══╝  ╘═╤═╝  ╘═╤═╝
--                        └──────┘
