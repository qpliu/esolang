-- An implementation of But Is It Art?
--
-- See: http://esolangs.org/wiki/But_Is_It_Art%3F
--
-- Since it's not clear whether multiple identical tiles in the source
-- need to appear once or multiple times in the witness rectangle, this
-- implementation allows them to appear once.  Hence, given the program
--
--   a a
--
-- and an input of one 0 nybble, this implementation will return success
-- with the witness rectangle
--
--   a
import Data.Array(Array,array,assocs,bounds,range,(!),(//))
import qualified Data.Array as A
import Data.Char(chr,ord)
import Data.List(find)
import Data.Set(Set,delete,empty,fromList,insert,member,union)
import qualified Data.Set as S
import System.Environment(getArgs)
import System.Exit(exitFailure)

newtype Tile = Tile (Array (Int,Int) Char) deriving (Ord, Eq)
newtype Grid = Grid (Array (Int,Int) Char) deriving Eq

showArray :: Array (Int,Int) Char -> String
showArray arr = unlines [[arr ! (y,x) | x <- [x0..x1]] | y <- [y0..y1]]
  where
    ((y0,x0),(y1,x1)) = bounds arr

instance Show Tile where
    show (Tile arr) = "Tile:\n" ++ showArray arr

instance Show Grid where
    show (Grid arr) = "Grid:\n" ++ showArray arr

parse :: String -> Set Tile
parse src = (fromList . fst . foldl findTile ([],empty) . range) gridBounds
  where
    srcLines = lines src
    width = maximum (map length srcLines)
    height = length srcLines
    gridLines = map (take width . (++ repeat ' ')) srcLines
    gridBounds = ((1,1),(height,width))
    srcGrid = array gridBounds (zip (range gridBounds) (concat gridLines))
    findTile (tiles,used) i
      | member i used || srcGrid ! i == ' ' = (tiles,used)
      | otherwise = let tileIndices = mapTile i empty
                    in  (makeTile tileIndices : tiles, union used tileIndices)
    mapTile i@(y,x) indices
      | x < 1 || x > width || y < 1 || y > height = indices
      | member i indices || srcGrid ! i == ' ' = indices
      | otherwise = (mapTile (y-1,x) . mapTile (y+1,x)
                   . mapTile (y,x-1) . mapTile (y,x+1)) (insert i indices)
    makeTile indices =
        Tile (blankArray tileBounds // map copyChar (S.elems indices))
      where
        y0 = (minimum . S.map fst) indices
        y1 = (maximum . S.map fst) indices
        x0 = (minimum . S.map snd) indices
        x1 = (maximum . S.map snd) indices
        tileBounds = ((0,0),(y1-y0,x1-x0))
        copyChar i@(y,x) = ((y-y0,x-x0),srcGrid ! i)

blankArray :: ((Int,Int),(Int,Int)) -> Array (Int,Int) Char
blankArray arrayBounds =
    array arrayBounds (zip (range arrayBounds) (repeat ' '))

openIndex :: Grid -> Maybe (Int,Int)
openIndex (Grid arr) = (find open . range . bounds) arr
  -- Upper-left most open space
  where
    open i = arr ! i == ' '

indicesToTry :: Grid -> [(Int,Int)]
indicesToTry grid@(Grid arr) =
    (maybe id (:) (openIndex grid)) (right ++ below)
  -- If grid is not full, try to fit in upper-left most open space
  -- then try extending from the top right and bottom left if connected.
  where
    ((y0,x0),(y1,x1)) = bounds arr
    right = if arr ! (y0,x1) == ' ' then [] else [(y0,x1+1)]
    below = if arr ! (y1,x0) == ' ' then [] else [(y1+1,x0)]

tryFit :: Grid -> Tile -> (Int,Int) -> Maybe Grid
tryFit (Grid grid) (Tile tile) (gy,gx)
  -- Try fitting the left-most character in the top row of the tile
  -- at the given index.  Fail if non-space characters overlap or if
  -- the tile extends left of the grid.
  | newgy0 < gy0 || newgx0 < gx0 = Nothing
  | otherwise = fmap Grid (foldl (maybe (const Nothing) fit)
                                 (Just newGrid0) (assocs tile))
  where
    ((gy0,gx0),(gy1,gx1)) = bounds grid
    ((ty0,tx0),(ty1,tx1)) = bounds tile
    Just tx = find (\ x -> tile ! (ty0,x) /= ' ') [tx0..tx1]
    xlatey tiley = tiley+gy-ty0
    xlatex tilex = tilex+gx-tx
    xlate (tiley,tilex) = (xlatey tiley,xlatex tilex)
    newgy0 = min gy0 (xlatey ty0)
    newgy1 = max gy1 (xlatey ty1)
    newgx0 = min gx0 (xlatex tx0)
    newgx1 = max gx1 (xlatex tx1)
    newGrid0 = blankArray ((newgy0,newgx0),(newgy1,newgx1)) // assocs grid
    fit newGrid (i,char)
      | char == ' ' = Just newGrid
      | newGrid ! xlate i == ' ' = Just (newGrid // [(xlate i,char)])
      | otherwise = Nothing

witnessInput :: Grid -> String
witnessInput (Grid grid) = filter (flip elem ['a'..'p']) (A.elems grid)

witnessInputImpossible :: Grid -> String -> Bool
witnessInputImpossible (Grid grid) input =
    (or . zipWith (/=) input
        .  filter (flip elem ['a'..'p']) . takeWhile (/= ' ') . A.elems) grid

witnessOutput :: Grid -> String
witnessOutput (Grid grid) = filter (flip elem ['A'..'P']) (A.elems grid)

search :: Set Tile -> String -> [(Set Tile,Grid,Tile,(Int,Int))] -> Maybe Grid
search tiles input [] = Nothing
search tiles input ((lastUnused,lastGrid,tile,i):rest)
  -- Breadth-first search
  | maybeGrid == Nothing = search tiles input rest
  | length (witnessInput grid) > length input = search tiles input rest
  | witnessInputImpossible grid input = search tiles input rest
  | unused == empty && witnessInput grid == input
                    && openIndex grid == Nothing = Just grid
  | otherwise = search tiles input
                    (rest ++ [(unused,grid,t,i) | t <- S.elems tiles,
                                                  i <- indicesToTry grid])
  where
    maybeGrid = tryFit lastGrid tile i
    Just grid = maybeGrid
    unused = delete tile lastUnused

run :: String -> String -> Maybe Grid
run src input
  -- Easy tests for non-existence of witness
  | not (all (flip elem src) input) = Nothing
  | all upperLeft tiles = Nothing
  | all upperRight tiles = Nothing
  | all lowerLeft tiles = Nothing
  | all lowerRight tiles = Nothing
  | otherwise = search tiles input initialList
  where
    initialGrid = Grid (array ((0,0),(-1,-1)) [])
    tiles = parse src
    upperLeft  (Tile t) = let ((y,x),(_,_)) = bounds t in t ! (y,x) == ' '
    upperRight (Tile t) = let ((y,_),(_,x)) = bounds t in t ! (y,x) == ' '
    lowerLeft  (Tile t) = let ((_,x),(y,_)) = bounds t in t ! (y,x) == ' '
    lowerRight (Tile t) = let ((_,_),(y,x)) = bounds t in t ! (y,x) == ' '
    initialList = [(tiles,initialGrid,tile,(0,0)) | tile <- S.elems tiles]

encodeInput :: String -> String
encodeInput = concatMap encodeByte
  where
    encodeByte c = [chr (ord 'a' + (ord c `div` 16) `mod` 16),
                    chr (ord 'a' + (ord c `mod` 16))]

decodeOutput :: String -> String
decodeOutput (c1:c2:rest) =
    chr ((ord c1 - ord 'A') * 16 + (ord c2 - ord 'A')) : decodeOutput rest
decodeOutput _ = []

main :: IO ()
main = do
    (srcFile:_) <- getArgs
    src <- readFile srcFile
    input <- fmap encodeInput getContents
    maybe exitFailure (putStr . decodeOutput . witnessOutput) (run src input)
