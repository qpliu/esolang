-- https://esolangs.org/wiki/Funciton

-- Minimal implementation.  Invalid code will result in unhelpful error
-- messages, stack overflows, infinite loops, or other unexpected results.
--
-- Usage: funkytown SRC-FILE [SRC-FILE ...]
-- Build: ghc --make funkytown
--
-- Possible limitation:
-- Valid usage of lambdas may result in stack overflows or infinite loops
-- due to implementing closures as a stack frame without any caller stack
-- frames, which requires forcing the evaluation of the function inputs
-- at the time of the lambda creation rather than lazily evaluating them
-- possibly not until after the time of the lambda invocation.

import Data.Array(Array,array,assocs,bounds,inRange,(!))
import Data.Bits(complement,shift,(.&.))
import Data.Char(chr,ord)
import Data.List(partition)
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
    declId :: (Int,Int),
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
    makeDeclaration graph = Data.Map.fold buildDeclaration Declaration{declName=Nothing,declScope=scope,declPrivate=False,declId=(0,0),declInputs=Data.Set.empty,declOutputs=Data.Set.empty} graph

    buildDeclaration :: Box -> Declaration -> Declaration
    buildDeclaration Box{boxType=boxType,boxId=boxId,boxPrivate=boxPrivate,boxContents=boxContents,boxN=n,boxS=s,boxE=e,boxW=w} decl =
        checkHeader (foldl buildOutput decl [n,s,e,w])
      where
        buildOutput decl@Declaration{declOutputs=outputs,declInputs=inputs} (BoxConnExit dir)
          | Data.Set.member dir outputs = error "Invalid function: duplicate output"
          | Data.Set.member dir inputs = error "Invalid function: input-output overlap"
          | otherwise = decl{declId=boxId,declOutputs=Data.Set.insert dir outputs}
        buildOutput decl dir = decl{declId=boxId}
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
  | ExprExpr ElementId ExprId
  | ExprNand ElementId ExprId ExprId
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
    show (ExprExpr _ a) = "expr(" ++ show a ++ ")"
    show (ExprNand _ a b) = "nand(" ++ show a ++ "," ++ show b ++ ")"
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

removeUnreachableFunctions :: ([Subprogram],Program) -> Program
removeUnreachableFunctions (initialMains,initialSubprograms) = Data.Map.filter ((`Data.Set.member` reachable) . decl) initialSubprograms
  where
    (_,initialReachable) = foldl markReachable (Data.Set.empty,[]) initialMains
    reachable = walkReachables (Data.Set.empty,initialReachable)
    walkReachables (walked,[]) = walked
    walkReachables (walked,listToWalk) = walkReachables (foldl markReachable (walked,[]) listToWalk)
    markReachable (walked,listToWalk) subprogram@Subprogram{decl=decl,exprs=exprs}
      | decl `Data.Set.member` walked = (walked,listToWalk)
      | otherwise = (Data.Set.insert decl walked,map (initialSubprograms Data.Map.!) (Data.Map.fold listCallees [] exprs) ++ listToWalk)
    listCallees (ExprCall _ decl _ _ _) callees = decl:callees
    listCallees _ callees = callees

inliner :: ([Subprogram],Program) -> ([Subprogram],Program)
inliner (initialMains,initialSubprograms) = (initialMains,removeUnreachableFunctions (initialMains,initialSubprograms))
-- undefined iteratively inline subprograms that do not call any subprograms

optimizer :: Subprogram -> Subprogram
optimizer subprogram = subprogram
-- undefined remove ExprExprs (due to splitters and inlined cross-NOP calls)

parse :: [(String,String)] -> ([Subprogram],Program)
parse prog
  | not (null declErrs) = error (unlines declErrs)
  | not (null flowErrs) = error (unlines flowErrs)
  | otherwise = (optimizedMains,optimizedSubprograms)
  where
    declarations = concatMap parseDeclarations prog
    declErrs = declarationErrors (map fst declarations)
    flows = resolveFlows declarations
    flowErrs = concatMap flowErrors flows
    (mains,subprograms) = partition isProgramOutput (map resolve flows)
    (inlinedMains,inlinedSubprograms) = inliner (mains,Data.Map.fromList [(decl,subprogram) | subprogram@Subprogram{decl=decl} <- subprograms])
    (optimizedMains,optimizedSubprograms) = (map optimizer inlinedMains,Data.Map.map optimizer inlinedSubprograms)

    isProgramOutput Subprogram{decl=Declaration{declName=Nothing}} = True
    isProgramOutput _ = False

data Frame = Frame {
    subprogram :: Subprogram,
    inputs :: Map Dir LazyValue,
    values :: Map ExprId Value,
    lambdaIds :: Map ElementId Integer,
    lambdaInputs :: Map ElementId LazyValue,
    calleeFrames :: Map ElementId Frame
    } deriving Show

data Value = Value Integer (Map Integer Closure) deriving Show

data Closure = Closure ElementId (ExprId,ExprId) Frame deriving Show

class ValueHolder a where
    isValue :: a -> Bool
    getValue :: a -> Value

instance ValueHolder Value where
    isValue value = True
    getValue value = value

data LazyValue = LazyValue Value | LazyThunk ExprId | LazyInput LazyValue deriving Show

instance ValueHolder LazyValue where
    isValue (LazyValue _) = True
    isValue (LazyInput input) = isValue input
    isValue _ = False
    getValue (LazyValue value) = value
    getValue (LazyInput input) = getValue input
    getValue _ = error "getValue thunk"

data ForceResult = ForceValue Value | ForceNeeds LazyValue deriving Show

instance ValueHolder ForceResult where
    isValue (ForceValue _) = True
    isValue _ = False
    getValue (ForceValue value) = value
    getValue _ = error "getValue thunk"

data State = State {
    nextLambda :: Integer,
    stdin :: Integer,
    program :: Program,
    frame :: Frame
    }

eval :: State -> ExprId -> (State,LazyValue)
eval state@State{frame=frame@Frame{subprogram=subprogram@Subprogram{exprs=exprs},inputs=inputs,values=values}} exprId
  | exprId `Data.Map.member` values = (state,LazyValue (values Data.Map.! exprId))
  | otherwise = assign (evalExpr (exprs Data.Map.! exprId))
  where
    assign (state@State{frame=frame@Frame{values=values}},value)
      | isValue value = (state{frame=frame{values=Data.Map.insert exprId (getValue value) values}},value)
      | otherwise = (state,value)
    evalExpr (ExprConst Nothing) = (state,LazyValue (Value (stdin state) Data.Map.empty))
    evalExpr (ExprConst (Just n)) = (state,LazyValue (Value n Data.Map.empty))
    evalExpr (ExprInput dir) = (state,inputs Data.Map.! dir)
    evalExpr (ExprCall elementId decl params resultDir _) = (state,LazyThunk exprId)
    evalExpr (ExprExpr elementId exprId) = eval state exprId -- should be removed by optimizer
    evalExpr (ExprNand elementId aExprId bExprId)
      | isValue a && aVal == 0 = (state1,LazyValue (Value (-1) aClosures))
      | isValue a && isValue b = (state2,LazyValue (Value (complement (aVal .&. bVal)) (Data.Map.union aClosures bClosures)))
      | otherwise = (state2,LazyThunk exprId)
      where
        (state1,a) = eval state aExprId
        (state2,b) = eval state1 bExprId
        Value aVal aClosures = getValue a
        Value bVal bClosures = getValue b
    evalExpr (ExprLessThan elementId aExprId bExprId)
      | isValue a && isValue b = (state2,LazyValue (Value (if aVal < bVal then -1 else 0) Data.Map.empty))
      | otherwise = (state2,LazyThunk exprId)
      where
        (state1,a) = eval state aExprId
        (state2,b) = eval state1 bExprId
        Value aVal aClosures = getValue a
        Value bVal bClosures = getValue b
    evalExpr (ExprShiftLeft elementId aExprId bExprId)
      | isValue a && isValue b = (state2,LazyValue (Value (shiftInteger aVal bVal) (Data.Map.union aClosures bClosures)))
      | otherwise = (state,LazyThunk exprId)
      where
        (state1,a) = eval state aExprId
        (state2,b) = eval state1 bExprId
        Value aVal aClosures = getValue a
        Value bVal bClosures = getValue b
    evalExpr (ExprLambda elementId output1ExprId output2ExprId) = (state,LazyThunk exprId)
    evalExpr (ExprInvokeLambda1 elementId lambdaExprId inputExprId) = (state,LazyThunk exprId)
    evalExpr (ExprInvokeLambda2 elementId lambdaExprId inputExprId) = (state,LazyThunk exprId)

forceStep :: State -> ExprId -> (State,ForceResult)
forceStep state@State{frame=frame@Frame{subprogram=subprogram@Subprogram{exprs=exprs},inputs=inputs,values=values,lambdaInputs=lambdaInputs,calleeFrames=calleeFrames}} exprId
  | exprId `Data.Map.member` values = (state,ForceValue (values Data.Map.! exprId))
  | otherwise = assign (forceExpr (exprs Data.Map.! exprId))
  where
    assign (state@State{frame=frame@Frame{values=values}},value)
      | isValue value = (state{frame=frame{values=Data.Map.insert exprId (getValue value) values}},value)
      | otherwise = (state,value)
    forceExpr (ExprConst Nothing) = (state,ForceValue (Value (stdin state) Data.Map.empty))
    forceExpr (ExprConst (Just n)) = (state,ForceValue (Value n Data.Map.empty))
    forceExpr (ExprInput dir)
      | isValue input = (state,ForceValue (getValue input))
      | otherwise = (state,ForceNeeds input)
      where input = inputs Data.Map.! dir
    forceExpr expr@(ExprCall _ _ _ _ _) = forceStepCall state expr
    forceExpr (ExprExpr elementId exprId) = forceStep state exprId
    forceExpr (ExprNand elementId aExprId bExprId)
      | not (isValue aForce) = (state1,aForce)
      | aVal == 0 = (state1,ForceValue (Value (-1) aClosures))
      | not (isValue bForce) = (state2,bForce)
      | otherwise = (state2,ForceValue (Value (complement (aVal .&. bVal)) (Data.Map.union aClosures bClosures)))
      where
        (state1,aForce) = forceStep state aExprId
        (state2,bForce) = forceStep state1 bExprId
        ForceValue (Value aVal aClosures) = aForce
        ForceValue (Value bVal bClosures) = bForce
    forceExpr (ExprLessThan elementId aExprId bExprId)
      | not (isValue aForce) = (state1,aForce)
      | not (isValue bForce) = (state2,bForce)
      | otherwise = (state2,ForceValue (Value (if aVal < bVal then -1 else 0) Data.Map.empty))
      where
        (state1,aForce) = forceStep state aExprId
        (state2,bForce) = forceStep state1 bExprId
        ForceValue (Value aVal aClosures) = aForce
        ForceValue (Value bVal bClosures) = bForce
    forceExpr (ExprShiftLeft elementId aExprId bExprId)
      | not (isValue aForce) = (state1,aForce)
      | not (isValue bForce) = (state2,bForce)
      | otherwise = (state2,ForceValue (Value (shiftInteger aVal bVal) (Data.Map.union aClosures bClosures)))
      where
        (state1,aForce) = forceStep state aExprId
        (state2,bForce) = forceStep state1 bExprId
        ForceValue (Value aVal aClosures) = aForce
        ForceValue (Value bVal bClosures) = bForce
    forceExpr expr@(ExprLambda _ _ _) = forceStepLambda state expr
    forceExpr (ExprLambdaInput elementId)
      | isValue input = (state,ForceValue (getValue input))
      | otherwise = (state,ForceNeeds input)
      where input = lambdaInputs Data.Map.! elementId
    forceExpr (ExprInvokeLambda1 elementId lambdaExprId inputExprId) =
        forceStepInvokeLambda state elementId lambdaExprId inputExprId fst
    forceExpr (ExprInvokeLambda2 elementId lambdaExprId inputExprId) =
        forceStepInvokeLambda state elementId lambdaExprId inputExprId snd

forceStepCall :: State -> Expr -> (State,ForceResult)
forceStepCall callerState expr@(ExprCall elementId calleeDecl params resultDir _) =
    handleCallResult (forceStep callerStateBeforeCall{frame=initialCalleeFrame} (outputs (subprogram initialCalleeFrame) Data.Map.! resultDir))
  where
    (callerStateBeforeCall,initialCalleeFrame) = maybe (newFrame callerStateAfterEvalArgs (program callerStateAfterEvalArgs Data.Map.! calleeDecl) args) (\ frame -> (callerStateAfterEvalArgs,frame{inputs=args})) (Data.Map.lookup elementId (calleeFrames (frame callerStateAfterEvalArgs)))
    (callerStateAfterEvalArgs,args) = Data.Map.foldWithKey evalArg (callerState,Data.Map.empty) params
    evalArg paramDir paramExprId (callerStateEvalArg,args) = fmap ((flip (Data.Map.insert paramDir) args) . LazyInput) (eval callerStateEvalArg paramExprId) 

    -- replace callee frame with caller frame with callee frame in calleeFrames Map
    finalCallerState :: State -> State
    finalCallerState calleeReturnedState@State{frame=calleeReturnedFrame} = calleeReturnedState{frame=(frame callerStateBeforeCall){calleeFrames=Data.Map.insert elementId calleeReturnedFrame (calleeFrames (frame callerStateBeforeCall))}}

    handleCallResult :: (State,ForceResult) -> (State,ForceResult)
    handleCallResult (calleeReturnedState,callResult@(ForceValue _)) = (finalCallerState calleeReturnedState,callResult)
    handleCallResult (calleeReturnedState,ForceNeeds (LazyInput neededInput)) = (finalCallerState calleeReturnedState,ForceNeeds neededInput)
    handleCallResult (calleeReturnedState,ForceNeeds (LazyThunk calleeExprId))
      | isValue recursiveResult = forceStepCall (finalCallerState calleeReturnedState2) expr
      | otherwise = handleCallResult (calleeReturnedState2,recursiveResult)
      where
        (calleeReturnedState2,recursiveResult) = recursiveForceStep calleeReturnedState calleeExprId

recursiveForceStep :: State -> ExprId -> (State,ForceResult)
recursiveForceStep state exprId = handleResult (forceStep state exprId)
  where
    handleResult (state,ForceNeeds (LazyThunk nextExprId)) = retry (recursiveForceStep state nextExprId)
    handleResult (state,result) = (state,result)
    retry (nextState,result)
      | isValue result = recursiveForceStep nextState exprId
      | otherwise = (nextState,result)

forceStepLambda :: State -> Expr -> (State,ForceResult)
forceStepLambda state@State{frame=frame@Frame{inputs=inputs,lambdaIds=lambdaIds}} expr@(ExprLambda elementId output1ExprId output2ExprId) = checkInputs (filter (not . isValue) (Data.Map.elems inputs))
  where
    checkInputs (arg:_) = (state,ForceNeeds arg)
    checkInputs [] = (state,ForceValue (Value lambdaId (Data.Map.fromList [(lambdaId,Closure elementId (output1ExprId,output2ExprId) frame)])))
    lambdaId = lambdaIds Data.Map.! elementId

forceStepInvokeLambda :: State -> ElementId -> ExprId -> ExprId -> ((ExprId,ExprId) -> ExprId) -> (State,ForceResult)
forceStepInvokeLambda state invokeElementId lambdaExprId inputExprId chooseOutput
  | not (isValue lambdaResult) = (stateAfterEvalLambda,lambdaResult)
  | otherwise = handleCallResult (forceStep stateAfterEvalArg{frame=lambdaFrame} (chooseOutput outputExprIds))
  where
    (stateAfterEvalLambda,lambdaResult) = recursiveForceStep state lambdaExprId
    ForceValue (Value lambdaId closures) = lambdaResult

    (stateAfterEvalArg,arg) = eval stateAfterEvalLambda inputExprId
    Closure closureElementId outputExprIds closureFrame = closures Data.Map.! lambdaId
    initialLambdaFrame = maybe closureFrame id (Data.Map.lookup invokeElementId (calleeFrames (frame stateAfterEvalLambda)))
    lambdaFrame = initialLambdaFrame{lambdaInputs=Data.Map.insert closureElementId arg (lambdaInputs initialLambdaFrame)}

    finalCallerState :: State -> State
    finalCallerState calleeReturnedState@State{frame=calleeReturnedFrame} = calleeReturnedState{frame=(frame stateAfterEvalArg){calleeFrames=Data.Map.insert invokeElementId calleeReturnedFrame (calleeFrames (frame stateAfterEvalArg))}}

    handleCallResult :: (State,ForceResult) -> (State,ForceResult)
    handleCallResult (calleeReturnedState,callResult@(ForceValue _)) = (finalCallerState calleeReturnedState,callResult)
    handleCallResult (calleeReturnedState,ForceNeeds (LazyInput neededInput)) = (finalCallerState calleeReturnedState,ForceNeeds neededInput)
    handleCallResult (calleeReturnedState,ForceNeeds (LazyThunk calleeExprId))
      | isValue recursiveResult = forceStepInvokeLambda (finalCallerState calleeReturnedState2) invokeElementId lambdaExprId inputExprId chooseOutput
      | otherwise = handleCallResult (calleeReturnedState2,recursiveResult)
      where
        (calleeReturnedState2,recursiveResult) = recursiveForceStep calleeReturnedState calleeExprId

shiftInteger :: Integer -> Integer -> Integer
shiftInteger a b
  | b > fromIntegral (maxBound :: Int) = shiftInteger (shift a maxBound) (b - fromIntegral (maxBound :: Int))
  | b < fromIntegral (minBound :: Int) = shiftInteger (shift a minBound) (b - fromIntegral (minBound :: Int))
  | otherwise = shift a (fromIntegral b)

newFrame :: State -> Subprogram -> Map Dir LazyValue -> (State,Frame)
newFrame state@State{nextLambda=initialLambda} subprogram@Subprogram{exprs=exprs} args = (state{nextLambda=finalLambda},Frame{subprogram=subprogram,inputs=args,values=Data.Map.empty,lambdaIds=lambdaIds,lambdaInputs=Data.Map.empty,calleeFrames=Data.Map.empty})
  where
    (finalLambda,lambdaIds) = Data.Map.fold enumerateLambdas (initialLambda,Data.Map.empty) exprs
    enumerateLambdas (ExprLambda elementId _ _) (nextLambda,lambdaIds) = (nextLambda+1,Data.Map.insert elementId nextLambda lambdaIds)
    enumerateLambdas _ (nextLambda,lambdaIds) = (nextLambda,lambdaIds)

run :: [(String,String)] -> Integer -> [Integer]
run prog inp = concatMap evalMainOutputs mains
  where
    (mains,program) = parse prog
    evalMainOutputs subprogram@Subprogram{outputs=outputs} =
        map (evalMain subprogram) (Data.Map.elems outputs)
    evalMain subprogram exprId = result
      where
        uninitializedState = State{nextLambda=1,stdin=inp,program=program,frame=undefined}
        (initialState,initialFrame) = newFrame uninitializedState subprogram Data.Map.empty
        (_,ForceValue (Value result _)) = recursiveForceStep initialState{frame=initialFrame} exprId

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

dumpParse :: [String] -> IO ()
dumpParse srcFiles = do
    srcs <- mapM readFile srcFiles
    let flows = resolveFlows (concatMap parseDeclarations (zip srcFiles srcs))
    let (mains,subprograms) = parse (zip srcFiles srcs)
    mapM_ print mains
    mapM_ print subprograms
