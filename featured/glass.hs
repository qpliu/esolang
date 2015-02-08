-- http://esolangs.org/wiki/Glass

import Data.Array(Array,array,bounds,inRange)
import Data.Char(chr,isAlpha,isDigit,isLower,isSpace,isUpper,ord)
import Data.Fixed(mod')
import Data.List(nub)
import Data.Map(Map,delete,elems,fromList,member,(!))



-- tokenize

data Token =
    TokenName String
  | TokenStackReference Int
  | TokenString String
  | TokenNumber Float
  | TokenCommand Char
  deriving Show


tokenize :: String -> [Token]
tokenize [] = []
tokenize ('(':str)
  | all isDigit val = TokenStackReference (read val) : tokenize rest
  | otherwise = TokenName val : tokenize rest
  where (val,')':rest) = break (== ')') str
tokenize ('"':str) = TokenString (newlineUnesc val) : tokenize rest
  where (val,'"':rest) = break (== '"') str
        newlineUnesc "" = ""
        newlineUnesc ('\\':'n':cs) = '\n':newlineUnesc cs
        newlineUnesc (c:cs) = c:newlineUnesc cs
tokenize ('<':str) = TokenNumber (read val) : tokenize rest
  where (val,'>':rest) = break (== '>') str
tokenize ('\'':str) = tokenize rest
  where (val,'\'':rest) = break (== '\'') str
tokenize (ch:rest)
  | isAlpha ch = TokenName [ch] : tokenize rest
  | isDigit ch = TokenStackReference (read [ch]) : tokenize rest
  | isSpace ch = tokenize rest
  | otherwise = TokenCommand ch : tokenize rest



-- parse

data Class = Class String [Function]
  deriving Show

data Function =
    Function String [Command]
  | FunctionBuiltin String Op

instance Show Function where
    show (Function name cmds) = "Function " ++ show name ++ " " ++ show cmds
    show (FunctionBuiltin name _) = "FunctionBuiltin " ++ show name

data Command =
    CommandPushName String
  | CommandDup Int
  | CommandPushStr String
  | CommandPushNum Float
  | CommandPop
  | CommandRet
  | CommandAssign
  | CommandNew
  | CommandLookupFunc
  | CommandCallFunc
  | CommandLookupVar
  | CommandAssignObj
  | CommandLoop String [Command]
  deriving Show


parse :: [Token] -> [Class]
parse [] = []
parse (TokenCommand '{':tokens) = parseClass tokens
parse (_:tokens) = error "parse error"

parseClass :: [Token] -> [Class]
parseClass (TokenName className@(c:_):tokens)
  | isUpper c = parseFunctions className [] tokens
  | otherwise = parse tokens -- error
parseClass tokens = error "parse error"

parseFunctions :: String -> [Function] -> [Token] -> [Class]
parseFunctions className fns (TokenCommand '}':tokens) =
    Class className (reverse fns) : parse tokens
parseFunctions className fns (TokenCommand '[':tokens) =
    parseFunction className fns tokens
parseFunctions className fns (_:tokens) = parseFunctions className fns tokens

parseFunction :: String -> [Function] -> [Token] -> [Class]
parseFunction className fns (TokenName fnName@(c:_):tokens)
  | isLower c = parseCommands (parseEndFunction className fns fnName) [] tokens
  | otherwise = error "parse error"
parseFunction _ _ tokens = error "parse error"

parseEndFunction :: String -> [Function] -> String -> [Command] -> [Token] -> [Class]
parseEndFunction className fns fnName cmds (TokenCommand ']':tokens) =
    parseFunctions className (Function fnName cmds:fns) tokens
parseEndFunction _ _ _ _ tokens = error "parse error"

parseCommands :: ([Command] -> [Token] -> [Class]) -> [Command] -> [Token] -> [Class]
parseCommands endCmds cmds (TokenName name:tokens) =
    parseCommands endCmds (CommandPushName name:cmds) tokens
parseCommands endCmds cmds (TokenStackReference i:tokens) =
    parseCommands endCmds (CommandDup i:cmds) tokens
parseCommands endCmds cmds (TokenString str:tokens) =
    parseCommands endCmds (CommandPushStr str:cmds) tokens
parseCommands endCmds cmds (TokenNumber num:tokens) =
    parseCommands endCmds (CommandPushNum num:cmds) tokens
parseCommands endCmds cmds (TokenCommand ',':tokens) =
    parseCommands endCmds (CommandPop:cmds) tokens
parseCommands endCmds cmds (TokenCommand '^':tokens) =
    parseCommands endCmds (CommandRet:cmds) tokens
parseCommands endCmds cmds (TokenCommand '=':tokens) =
    parseCommands endCmds (CommandAssign:cmds) tokens
parseCommands endCmds cmds (TokenCommand '!':tokens) =
    parseCommands endCmds (CommandNew:cmds) tokens
parseCommands endCmds cmds (TokenCommand '.':tokens) =
    parseCommands endCmds (CommandLookupFunc:cmds) tokens
parseCommands endCmds cmds (TokenCommand '?':tokens) =
    parseCommands endCmds (CommandCallFunc:cmds) tokens
parseCommands endCmds cmds (TokenCommand '*':tokens) =
    parseCommands endCmds (CommandLookupVar:cmds) tokens
parseCommands endCmds cmds (TokenCommand '$':tokens) =
    parseCommands endCmds (CommandAssignObj:cmds) tokens
parseCommands endCmds cmds (TokenCommand '/':TokenName varName:tokens) =
    parseCommands (parseEndLoop endCmds cmds varName) [] tokens
parseCommands endCmds cmds tokens = endCmds (reverse cmds) tokens

parseEndLoop :: ([Command] -> [Token] -> [Class]) -> [Command] -> String -> [Command] -> [Token] -> [Class]
parseEndLoop endCmds cmds varName loopCmds (TokenCommand '\\':tokens) =
    parseCommands endCmds (CommandLoop varName loopCmds:cmds) tokens
parseEndLoop _ _ _ _ tokens = error "parse error"



-- compile

type CClass = Map String CFunc

type CFunc = Array Int Op

type Op = (State -> String -> String) -> State -> String -> String


compile :: [Class] -> Map String CClass
compile = fromList . map compileClass

compileClass :: Class -> (String,CClass)
compileClass (Class name fns) = (name,(fromList . map compileFn) fns)

compileFn :: Function -> (String,CFunc)
compileFn (Function name cmds) = (name,uncurry array (compileCmds 0 cmds []))
compileFn (FunctionBuiltin name op) = (name,array (0,0) [(0,op)])

compileCmds :: Int -> [Command] -> [(Int,Op)] -> ((Int,Int),[(Int,Op)])
compileCmds i [] ops = ((0,i-1),ops)
compileCmds i (CommandPushName name:cmds) ops =
    compileCmds (i+1) cmds ((i,cmdPushName name):ops)
compileCmds i (CommandDup j:cmds) ops =
    compileCmds (i+1) cmds ((i,cmdDup j):ops)
compileCmds i (CommandPushStr str:cmds) ops =
    compileCmds (i+1) cmds ((i,cmdPushStr str):ops)
compileCmds i (CommandPushNum num:cmds) ops =
    compileCmds (i+1) cmds ((i,cmdPushNum num):ops)
compileCmds i (CommandPop:cmds) ops =
    compileCmds (i+1) cmds ((i,cmdPop):ops)
compileCmds i (CommandRet:cmds) ops =
    compileCmds (i+1) cmds ((i,cmdRet):ops)
compileCmds i (CommandAssign:cmds) ops =
    compileCmds (i+1) cmds ((i,cmdAssign):ops)
compileCmds i (CommandNew:cmds) ops =
    compileCmds (i+1) cmds ((i,cmdNew):ops)
compileCmds i (CommandLookupFunc:cmds) ops =
    compileCmds (i+1) cmds ((i,cmdLookupFunc):ops)
compileCmds i (CommandCallFunc:cmds) ops =
    compileCmds (i+1) cmds ((i,cmdCallFunc):ops)
compileCmds i (CommandLookupVar:cmds) ops =
    compileCmds (i+1) cmds ((i,cmdLookupVar):ops)
compileCmds i (CommandAssignObj:cmds) ops =
    compileCmds (i+1) cmds ((i,cmdAssignObj):ops)
compileCmds i (CommandLoop name loopCmds:cmds) ops =
    compileCmds (j+2) cmds ((i,cmdJump (j+1)):(j+1,cmdIfTrue name (i+1)):ops')
  where ((_,j),ops') = compileCmds (i+1) loopCmds ops



-- commands

cmdPushName :: String -> Op
cmdPushName name@(ch:_) cont st@State{stStack = stack} inp
  | isUpper ch = incPC cont st{stStack = ValGlobalName name:stack} inp
  | isLower ch = incPC cont st{stStack = ValInstanceName name:stack} inp
  | ch == '_' = incPC cont st{stStack = ValLocalName name:stack} inp

cmdDup :: Int -> Op
cmdDup i cont st@State{stStack = stack} inp =
    incPC cont st{stStack = head (drop i stack):stack} inp

cmdPushStr :: String -> Op
cmdPushStr str cont st@State{stStack = stack} inp =
    incPC cont st{stStack = ValString str:stack} inp

cmdPushNum :: Float -> Op
cmdPushNum num cont st@State{stStack = stack} inp =
    incPC cont st{stStack = ValNum num:stack} inp

cmdPop :: Op
cmdPop cont st@State{stStack = stack} inp =
    incPC cont st{stStack = drop 1 stack} inp

cmdRet :: Op
cmdRet cont st@State{stGCFlag = flag, stCallStack = frame:frames} inp =
    cont' st{stCallStack = frames} inp
  where cont' | null frames = cont
              | otherwise = (setGCFlag True . gc . setGCFlag flag) cont

cmdAssign :: Op
cmdAssign = undefined

cmdNew :: Op
cmdNew = undefined

cmdLookupFunc :: Op
cmdLookupFunc = undefined

cmdCallFunc :: Op
cmdCallFunc cont st@State{stStack = ValFunc frame:stack, stCallStack = frames} inp =
    cont st{stStack = stack, stCallStack = frame:frames} inp

cmdLookupVar :: Op
cmdLookupVar = undefined

cmdAssignObj :: Op
cmdAssignObj = undefined

cmdJump :: Int -> Op
cmdJump dest cont st@State{stCallStack = frame:frames} inp =
    incPC cont st{stCallStack = frame{cfPC = dest}:frames} inp

cmdIfTrue :: String -> Int -> Op
cmdIfTrue = undefined

incPC :: Op
incPC cont st@State{stCallStack = frame@CallFrame{cfOps = ops, cfPC = pc}:frames} inp
  | bounds ops `inRange` (pc + 1) =
        cont st{stCallStack = frame{cfPC = pc+1}:frames} inp
  | otherwise = cmdRet cont st inp



-- builtins

builtins :: [Class]
builtins = [
    Class "A" [
        FunctionBuiltin "a"
            (\ cont st@State{stStack = ValNum y:ValNum x:stack} inp ->
                incPC cont st{stStack = ValNum (x + y):stack} inp),
        FunctionBuiltin "s"
            (\ cont st@State{stStack = ValNum y:ValNum x:stack} inp ->
                incPC cont st{stStack = ValNum (x - y):stack} inp),
        FunctionBuiltin "m"
            (\ cont st@State{stStack = ValNum y:ValNum x:stack} inp ->
                incPC cont st{stStack = ValNum (x * y):stack} inp),
        FunctionBuiltin "d"
            (\ cont st@State{stStack = ValNum y:ValNum x:stack} inp ->
                incPC cont st{stStack = ValNum (x / y):stack} inp),
        FunctionBuiltin "mod"
            (\ cont st@State{stStack = ValNum y:ValNum x:stack} inp ->
                incPC cont st{stStack = ValNum (mod' x y):stack} inp),
        FunctionBuiltin "f"
            (\ cont st@State{stStack = ValNum x:stack} inp ->
                incPC cont st{stStack = ValNum (fromIntegral $ floor x):stack} inp),
        FunctionBuiltin "e"
            (\ cont st@State{stStack = ValNum y:ValNum x:stack} inp ->
                incPC cont st{stStack = ValNum (if x == y then 1 else 0):stack} inp),
        FunctionBuiltin "ne"
            (\ cont st@State{stStack = ValNum y:ValNum x:stack} inp ->
                incPC cont st{stStack = ValNum (if x /= y then 1 else 0):stack} inp),
        FunctionBuiltin "lt"
            (\ cont st@State{stStack = ValNum y:ValNum x:stack} inp ->
                incPC cont st{stStack = ValNum (if x < y then 1 else 0):stack} inp),
        FunctionBuiltin "le"
            (\ cont st@State{stStack = ValNum y:ValNum x:stack} inp ->
                incPC cont st{stStack = ValNum (if x <= y then 1 else 0):stack} inp),
        FunctionBuiltin "gt"
            (\ cont st@State{stStack = ValNum y:ValNum x:stack} inp ->
                incPC cont st{stStack = ValNum (if x > y then 1 else 0):stack} inp),
        FunctionBuiltin "ge"
            (\ cont st@State{stStack = ValNum y:ValNum x:stack} inp ->
                incPC cont st{stStack = ValNum (if x >= y then 1 else 0):stack} inp)
        ],
    Class "S" [
        FunctionBuiltin "l"
            (\ cont st@State{stStack = ValString str:stack} inp ->
                incPC cont st{stStack = ValNum (fromIntegral $ length str):stack} inp),
        FunctionBuiltin "i"
            (\ cont st@State{stStack = ValNum n:ValString str:stack} inp ->
                incPC cont st{stStack = ValString (take 1 $ drop (round n) str):stack} inp),
        FunctionBuiltin "si"
            (\ cont st@State{stStack = ValString ch:ValNum n:ValString str:stack} inp ->
                incPC cont st{stStack = ValString (take (round n) str ++ ch ++ drop (round n + 1) str):stack} inp),
        FunctionBuiltin "a"
            (\ cont st@State{stStack = ValString s2:ValString s1:stack} inp ->
                incPC cont st{stStack = ValString (s1++s2):stack} inp),
        FunctionBuiltin "d"
            (\ cont st@State{stStack = ValNum n:ValString str:stack} inp ->
                incPC cont st{stStack = ValString (drop (round n) str):ValString (take (round n) str):stack} inp),
        FunctionBuiltin "e"
            (\ cont st@State{stStack = ValString s2:ValString s1:stack} inp ->
                incPC cont st{stStack = ValNum (if s1 == s2 then 1 else 0):stack} inp),
        FunctionBuiltin "ns"
            (\ cont st@State{stStack = ValNum n:stack} inp ->
                incPC cont st{stStack = ValString [chr $ round n]:stack} inp),
        FunctionBuiltin "sn"
            (\ cont st@State{stStack = ValString (ch:_):stack} inp ->
                incPC cont st{stStack = ValNum (fromIntegral $ ord ch):stack} inp)
        ],
    Class "V" [
        FunctionBuiltin "n"
            (\ cont st@State{stGlobals = globals, stStack = stack} inp ->
                let gen n | member ("Gen " ++ show n) globals = gen (n+1)
                          | otherwise = "Gen " ++ show n
                in  incPC cont st{stStack = ValGlobalName (gen 0):stack} inp),
        FunctionBuiltin "d"
            (\ cont st@State{stGlobals = globals, stStack = ValGlobalName name:stack} inp ->
                incPC cont st{stGlobals = delete name globals, stStack = stack} inp)
        ],
    Class "O" [
        FunctionBuiltin "o"
            (let p (ValGlobalName name) = name
                 p (ValInstanceName name) = name
                 p (ValLocalName name) = name
                 p (ValString str) = str
             in  \ cont st@State{stStack = val:stack} inp ->
                     p val ++ incPC cont st{stStack = stack} inp),
        FunctionBuiltin "on"
            (let p n | n == fromIntegral (round n) = show (round n) | otherwise = show n
             in  \ cont st@State{stStack = ValNum n:stack} inp ->
                     p n ++ incPC cont st{stStack = stack} inp)
        ],
    Class "I" [
        FunctionBuiltin "l"
            (\ cont st@State{stStack = stack} inp ->
                let (line,rest) = break (== '\n') inp
                in  incPC cont st{stStack = ValString line:stack} (drop 1 inp)),
        FunctionBuiltin "c"
            (\ cont st@State{stStack = stack} inp ->
                incPC cont st{stStack = ValString (take 1 inp):stack} (drop 1 inp)),
        FunctionBuiltin "e"
            (\ cont st@State{stStack = stack} inp ->
                incPC cont st{stStack = ValNum (if null inp then 1 else 0):stack} inp)
        ]
    ]



-- garbage collector

setGCFlag :: Bool -> Op
setGCFlag flag cont st inp = cont st{stGCFlag = flag} inp

gc :: Op
gc cont st@State{stGCFlag = True} inp = cont st inp -- running destructors or constructing M
gc cont st inp = cont st inp -- ... to be defined ...

gcRoots :: State -> [ObjRef]
gcRoots State{stGlobals = globals, stStack = stack, stCallStack = frames} =
    nub $ concatMap gcValRefs (elems globals) ++
          concatMap gcValRefs stack ++
          concatMap gcFrameRefs frames

gcValRefs :: Val -> [ObjRef]
gcValRefs (ValObj objRef) = [objRef]
gcValRefs (ValFunc frame) = gcFrameRefs frame
gcValRefs _ = []

gcFrameRefs :: CallFrame -> [ObjRef]
gcFrameRefs CallFrame{cfObj = obj, cfLocals = locals} =
    obj : concatMap gcValRefs (elems locals)

gcObjRefs :: Obj -> [ObjRef]
gcObjRefs Obj{objVars = vars} = concatMap gcValRefs (elems vars)



-- state

data State = State {
    stGlobals   :: Map String Val,
    stStack     :: [Val],
    stCallStack :: [CallFrame],
    stObjs      :: Map ObjRef Obj,
    stGCFlag    :: Bool
    }

data Val =
    ValGlobalName String
  | ValInstanceName String
  | ValLocalName String
  | ValString String
  | ValNum Float
  | ValObj ObjRef
  | ValFunc CallFrame
  | ValClass CClass

data CallFrame = CallFrame {
    cfOps    :: CFunc,
    cfPC     :: Int,
    cfObj    :: ObjRef,
    cfLocals :: Map String Val
    }

data Obj = Obj {
    objClass :: CClass,
    objVars  :: Map String Val
    }

newtype ObjRef = ObjRef Int deriving (Eq,Ord)



--

glass :: String -> IO ()
glass prog = undefined
