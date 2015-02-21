import Data.Char(chr,isDigit,isSpace,ord)
import Data.Map(Map)
import qualified Data.Map as M
import System.Environment(getArgs)

parse :: String -> Map Integer [(Expr,Expr)]
parse = foldl parseLine M.empty . lines

parseLine :: Map Integer [(Expr,Expr)] -> String -> Map Integer [(Expr,Expr)]
parseLine prog line
  | take 3 lineContents == "REM" = prog
  | null lineNum = error "SYNTAX ERROR"
  | take 4 lineContents /= "GOTO" = error "SYNTAX ERROR"
  | null postTarget =
      M.alter (Just . ((target,ExprN):) . maybe [] id) (read lineNum) prog
  | take 4 postTarget /= "WITH" = error "SYNTAX ERROR"
  | not (null trailingGarbage) = error "SYNTAX ERROR"
  | otherwise =
      M.alter (Just . ((target,withExpr):) . maybe [] id) (read lineNum) prog
  where
    (lineNum,lineContents) =
        break (not . isDigit) (filter (not . isSpace) line)
    (target,postTarget) = parseExpr (drop 4 lineContents)
    (withExpr,trailingGarbage) = parseExpr (drop 4 postTarget)

data Expr =
    ExprLiteral Integer
  | ExprAdd Expr Expr
  | ExprSub Expr Expr
  | ExprMul Expr Expr
  | ExprDiv Expr Expr
  | ExprI Expr
  | ExprN
  | ExprNAt Expr
  deriving Show

data Precedence = PrecAdd | PrecMul
  deriving Eq

parseExpr :: String -> (Expr,String)
parseExpr s = p2 PrecAdd (p s)
  where
    p str
      | take 2 str == "N(" = parenExpr 2 ExprNAt
      | take 1 str == "N" = (ExprN,drop 1 str)
      | take 2 str == "I(" = parenExpr 2 ExprI
      | take 1 str == "I" = (ExprI (ExprLiteral 1),drop 1 str)
      | take 1 str == "(" = parenExpr 1 id
      | not (null digits) = (ExprLiteral (read digits),postDigits)
      | otherwise = error "SYNTAX ERROR"
      where
        (digits,postDigits) = break (not . isDigit) str
        parenExpr ndrop f | take 1 rest /= ")" = error "SYNTAX ERROR"
                          | otherwise = (f expr,drop 1 rest)
            where (expr,rest) = parseExpr (drop ndrop str)
    p2 prec (lhs,str)
      | prec == PrecAdd && take 1 str == "+" =
          let (rhs,rest) = p2 PrecMul (p (drop 1 str))
          in  p2 PrecAdd ((ExprAdd lhs rhs),rest)
      | prec == PrecAdd && take 1 str == "-" =
          let (rhs,rest) = p2 PrecMul (p (drop 1 str))
          in  p2 PrecAdd ((ExprSub lhs rhs),rest)
      | take 1 str == "*" =
          let (rhs,rest) = p (drop 1 str)
          in  p2 prec ((ExprMul lhs rhs),rest)
      | take 1 str == "/" =
          let (rhs,rest) = p (drop 1 str)
          in  p2 prec ((ExprDiv lhs rhs),rest)
      | otherwise = (lhs,str)

eval :: (Integer,Map Integer Integer) -> Expr -> ([Bool],[Bool]) -> (Integer,([Bool],[Bool]))
eval _ (ExprLiteral x) inp = (x,inp)
eval threads (ExprAdd lexp rexp) inp = evalBinOp threads lexp rexp inp (+)
eval threads (ExprSub lexp rexp) inp = evalBinOp threads lexp rexp inp (-)
eval threads (ExprMul lexp rexp) inp = evalBinOp threads lexp rexp inp (*)
eval threads (ExprDiv lexp rexp) inp = evalBinOp threads lexp rexp inp div
eval threads (ExprI expr) inp = evalInput (eval threads expr inp)
eval (n,_) ExprN inp = (n,inp)
eval threads@(_,nmap) (ExprNAt expr) inp =
    let (fromLine,inp') = eval threads expr inp
    in  (maybe 0 id (M.lookup fromLine nmap),inp')

evalBinOp :: (Integer,Map Integer Integer) -> Expr -> Expr -> ([Bool],[Bool]) -> (Integer -> Integer -> Integer) -> (Integer,([Bool],[Bool]))
evalBinOp threads lexp rexp inp op = (op lval rval, rinp)
  where (lval,linp) = eval threads lexp inp
        (rval,rinp) = eval threads rexp linp

evalInput :: (Integer,([Bool],[Bool])) -> (Integer,([Bool],[Bool]))
evalInput (_,([],[])) = (-1,([],[]))
evalInput (n,inp@(inp1,inp2))
  | not (null inp2) && nbits > 0 =
      evalInput (n,(inp1 ++ take nbits inp2, drop nbits inp2))
  | otherwise = (fromBits (take (fromIntegral n) inp1),inp)
  where nbits = fromIntegral n - length inp1

fromBits :: Num a => [Bool] -> a
fromBits bits = (sum . map fst . filter snd) (zip (iterate (*2) 1) bits)

toBits :: Integral a => a -> a -> [Bool]
toBits scale num
  | scale <= 0 = []
  | otherwise = odd num : toBits (scale `div` 2) (num `div` 2)

fromStr :: String -> [Bool]
fromStr [] = []
fromStr (c:cs) = reverse (toBits 128 (ord c)) ++ fromStr cs

toStr :: [Bool] -> String
toStr b
  | null b = []
  | otherwise = (chr . fromBits . reverse . take 8) b : toStr (drop 8 b)

run :: Map Integer [(Expr,Expr)] -> Map Integer (Integer,Map Integer Integer) -> [Bool] -> [Bool]
run prog state inp
  | M.fold (+) 0 (M.map fst state) <= 0 = []
  | otherwise =
      (maybe [] output . fmap fst . M.lookup 0) state' ++ run prog state' inp'
  where
    (state',(_,inp')) = M.foldrWithKey gotos (M.empty,([],inp)) state
    output n | n == 0 = [] | n == 1 = [False] | otherwise = toBits (n-1) (n-1)
    gotos from threads@(nthread,_) st
      | nthread <= 0 = st
      | otherwise =
          foldl (goto from threads) st (maybe [] id (M.lookup from prog))
    goto from threads (state'',inp'') (targetExpr,withExpr) = (state''',inp''')
      where
        (target,tInp) = eval threads targetExpr inp''
        (withVal,inp''') = eval threads withExpr tInp
        state''' = M.alter (gotoTarget . maybe (0,M.empty) id) target state''
        gotoTarget (n,nmap) = Just (n+withVal,M.insert from withVal nmap)

goto10 :: String -> IO ()
goto10 prog = interact (toStr . run (parse prog) (M.fromList [(10,(1,M.empty))]) . fromStr)

main :: IO ()
main = do
    [srcFile] <- getArgs
    src <- readFile srcFile
    goto10 src
