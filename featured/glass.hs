-- http://esolangs.org/wiki/Glass

import Data.Array(Array)
import qualified Data.Array as A
import Data.Char(chr,isAlpha,isDigit,isLower,isSpace,isUpper,ord)
import Data.Fixed(mod')
import Data.Map(Map)
import qualified Data.Map as M
import Data.Set(Set)
import qualified Data.Set as S



-- tokenize

data Token =
    TokenName String
  | TokenStackReference Int
  | TokenString String
  | TokenNumber Float
  | TokenCommand Char


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

data Function =
    Function String [Command]
  | FunctionBuiltin String Op

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
compile = M.fromList . map compileClass

compileClass :: Class -> (String,CClass)
compileClass (Class name fns) = (name,(M.fromList . map compileFn) fns)

compileFn :: Function -> (String,CFunc)
compileFn (Function name cmds) = (name,uncurry A.array (compileCmds 0 cmds []))
compileFn (FunctionBuiltin name op) = (name,A.array (0,0) [(0,op)])

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
    compileCmds (j+2) cmds ((i,cmdJump j):(j+1,cmdIfTrue name i):ops')
  where ((_,j),ops') = compileCmds (i+1) loopCmds ops



-- commands

cmdPushName :: String -> Op
cmdPushName name cont st@State{stStack = stack} =
    incPC cont st{stStack = nameVal name:stack}

cmdDup :: Int -> Op
cmdDup i cont st@State{stStack = stack} =
    incPC cont st{stStack = head (drop i stack):stack}

cmdPushStr :: String -> Op
cmdPushStr str cont st@State{stStack = stack} =
    incPC cont st{stStack = ValString str:stack}

cmdPushNum :: Float -> Op
cmdPushNum num cont st@State{stStack = stack} =
    incPC cont st{stStack = ValNum num:stack}

cmdPop :: Op
cmdPop cont st@State{stStack = stack} =
    incPC cont st{stStack = drop 1 stack}

cmdRet :: Op
cmdRet cont st@State{stGCFlag = flag, stCallStack = frame:frames} =
    cont' st{stCallStack = frames}
  where cont' | null frames = cont
              | otherwise = (setGCFlag True . gc . setGCFlag flag . incPC) cont

cmdAssign :: Op
cmdAssign cont st@State{stStack = val:name:stack} =
    incPC cont (setVar st{stStack = stack} name val)

cmdNew :: Op
cmdNew cont st@State{stGCFlag = flag, stStack = cname:name:stack, stObjs = objs} =
    (runFunc cclass "c__" objRef . setGCFlag flag . incPC) cont st''
  where
    ValClass cclass = getVar st cname
    (objRef,st') = newObj cclass st
    st'' = setVar st'{stGCFlag = True, stStack = stack} name (ValObj objRef)

cmdLookupFunc :: Op
cmdLookupFunc cont st@State{stStack = ValInstanceName fname:oname:stack, stObjs = objs} =
    incPC cont st{stStack = ValFunc newFunc:stack}
  where
    ValObj objRef = getVar st oname
    Obj{objClass = cclass} = objs M.! objRef
    newFunc = CallFrame{cfOps = cclass M.! fname, cfPC = 0, cfObj = objRef, cfLocals = M.empty}

cmdCallFunc :: Op
cmdCallFunc cont st@State{stStack = ValFunc frame:stack, stCallStack = frames} =
    cont st{stStack = stack, stCallStack = frame:frames}

cmdLookupVar :: Op
cmdLookupVar cont st@State{stStack = name:stack} =
    incPC cont st{stStack = getVar st name:stack}

cmdAssignObj :: Op
cmdAssignObj cont st@State{stStack = name:stack, stCallStack = CallFrame{cfObj = objRef}:_} =
    incPC cont (setVar st{stStack = stack} name (ValObj objRef))

cmdJump :: Int -> Op
cmdJump dest cont st@State{stCallStack = frame:frames} =
    incPC cont st{stCallStack = frame{cfPC = dest}:frames}

cmdIfTrue :: String -> Int -> Op
cmdIfTrue name dest cont st@State{stCallStack = frame:frames}
  | testVal (getVar st (nameVal name)) =
        incPC cont st{stCallStack = frame{cfPC = dest}:frames}
  | otherwise = incPC cont st

incPC :: Op
incPC cont st@State{stCallStack = frame@CallFrame{cfOps = ops, cfPC = pc}:frames}
  | A.bounds ops `A.inRange` (pc + 1) =
        cont st{stCallStack = frame{cfPC = pc+1}:frames}
  | otherwise = cmdRet cont st



-- builtins

builtins :: [Class]
builtins = [
    Class "A" [
        FunctionBuiltin "a"
            (\ cont st@State{stStack = ValNum y:ValNum x:stack} ->
                incPC cont st{stStack = ValNum (x + y):stack}),
        FunctionBuiltin "s"
            (\ cont st@State{stStack = ValNum y:ValNum x:stack} ->
                incPC cont st{stStack = ValNum (x - y):stack}),
        FunctionBuiltin "m"
            (\ cont st@State{stStack = ValNum y:ValNum x:stack} ->
                incPC cont st{stStack = ValNum (x * y):stack}),
        FunctionBuiltin "d"
            (\ cont st@State{stStack = ValNum y:ValNum x:stack} ->
                incPC cont st{stStack = ValNum (x / y):stack}),
        FunctionBuiltin "mod"
            (\ cont st@State{stStack = ValNum y:ValNum x:stack} ->
                incPC cont st{stStack = ValNum (mod' x y):stack}),
        FunctionBuiltin "f"
            (\ cont st@State{stStack = ValNum x:stack} ->
                incPC cont st{stStack = ValNum (fromIntegral $ floor x):stack}),
        FunctionBuiltin "e"
            (\ cont st@State{stStack = ValNum y:ValNum x:stack} ->
                incPC cont st{stStack = boolVal (x == y):stack}),
        FunctionBuiltin "ne"
            (\ cont st@State{stStack = ValNum y:ValNum x:stack} ->
                incPC cont st{stStack = boolVal (x /= y):stack}),
        FunctionBuiltin "lt"
            (\ cont st@State{stStack = ValNum y:ValNum x:stack} ->
                incPC cont st{stStack = boolVal (x < y):stack}),
        FunctionBuiltin "le"
            (\ cont st@State{stStack = ValNum y:ValNum x:stack} ->
                incPC cont st{stStack = boolVal (x <= y):stack}),
        FunctionBuiltin "gt"
            (\ cont st@State{stStack = ValNum y:ValNum x:stack} ->
                incPC cont st{stStack = boolVal (x > y):stack}),
        FunctionBuiltin "ge"
            (\ cont st@State{stStack = ValNum y:ValNum x:stack} ->
                incPC cont st{stStack = boolVal (x >= y):stack})
        ],
    Class "S" [
        FunctionBuiltin "l"
            (\ cont st@State{stStack = ValString str:stack} ->
                incPC cont st{stStack = ValNum (fromIntegral $ length str):stack}),
        FunctionBuiltin "i"
            (\ cont st@State{stStack = ValNum n:ValString str:stack} ->
                incPC cont st{stStack = ValString (take 1 $ drop (round n) str):stack}),
        FunctionBuiltin "si"
            (\ cont st@State{stStack = ValString ch:ValNum n:ValString str:stack} ->
                incPC cont st{stStack = ValString (take (round n) str ++ ch ++ drop (round n + 1) str):stack}),
        FunctionBuiltin "a"
            (\ cont st@State{stStack = ValString s2:ValString s1:stack} ->
                incPC cont st{stStack = ValString (s1++s2):stack}),
        FunctionBuiltin "d"
            (\ cont st@State{stStack = ValNum n:ValString str:stack} ->
                incPC cont st{stStack = ValString (drop (round n) str):ValString (take (round n) str):stack}),
        FunctionBuiltin "e"
            (\ cont st@State{stStack = ValString s2:ValString s1:stack} ->
                incPC cont st{stStack = boolVal (s1 == s2):stack}),
        FunctionBuiltin "ns"
            (\ cont st@State{stStack = ValNum n:stack} ->
                incPC cont st{stStack = ValString [chr $ round n]:stack}),
        FunctionBuiltin "sn"
            (\ cont st@State{stStack = ValString (ch:_):stack} ->
                incPC cont st{stStack = ValNum (fromIntegral $ ord ch):stack})
        ],
    Class "V" [
        FunctionBuiltin "n"
            (\ cont st@State{stGlobals = globals, stStack = stack} ->
                let gen n | M.member ("Gen " ++ show n) globals = gen (n+1)
                          | otherwise = "Gen " ++ show n
                in  incPC cont st{stStack = ValGlobalName (gen 0):stack}),
        FunctionBuiltin "d"
            (\ cont st@State{stGlobals = globals, stStack = ValGlobalName name:stack} ->
                incPC cont st{stGlobals = M.delete name globals, stStack = stack})
        ],
    Class "O" [
        FunctionBuiltin "o"
            (let p (ValGlobalName name) = name
                 p (ValInstanceName name) = name
                 p (ValLocalName name) = name
                 p (ValString str) = str
             in  \ cont st@State{stStack = val:stack} input ->
                     p val ++ incPC cont st{stStack = stack} input),
        FunctionBuiltin "on"
            (let p n | n == fromIntegral (round n) = show (round n) | otherwise = show n
             in  \ cont st@State{stStack = ValNum n:stack} input ->
                     p n ++ incPC cont st{stStack = stack} input)
        ],
    Class "I" [
        FunctionBuiltin "l"
            (\ cont st@State{stStack = stack} input ->
                let (line,rest) = break (== '\n') input
                in  incPC cont st{stStack = ValString line:stack} rest),
        FunctionBuiltin "c"
            (\ cont st@State{stStack = stack} input ->
                incPC cont st{stStack = ValString (take 1 input):stack} (drop 1 input)),
        FunctionBuiltin "e"
            (\ cont st@State{stStack = stack} input ->
                incPC cont st{stStack = boolVal (null input):stack} input)
        ]
    ]



-- garbage collector

setGCFlag :: Bool -> Op
setGCFlag flag cont st = cont st{stGCFlag = flag}

gc :: Op
gc cont st@State{stGCFlag = True} = cont st -- running constructor or destructor
gc cont st = gcDestruct (S.toList garbage) (gcSweep garbage cont) st
  where garbage = gcGarbage st

gcRoots :: State -> [ObjRef]
gcRoots State{stGlobals = globals, stStack = stack, stCallStack = frames} =
    concatMap gcValRefs (M.elems globals)
        ++ concatMap gcValRefs stack ++ concatMap gcFrameRefs frames

gcValRefs :: Val -> [ObjRef]
gcValRefs (ValObj objRef) = [objRef]
gcValRefs (ValFunc frame) = gcFrameRefs frame
gcValRefs _ = []

gcFrameRefs :: CallFrame -> [ObjRef]
gcFrameRefs CallFrame{cfObj = obj, cfLocals = locals} =
    obj : concatMap gcValRefs (M.elems locals)

gcObjRefs :: State -> ObjRef -> [ObjRef]
gcObjRefs State{stObjs = objs} objRef = concatMap gcValRefs (M.elems vars)
  where Obj{objVars = vars} = objs M.! objRef

gcGarbage :: State -> Set ObjRef
gcGarbage st@State{stObjs = objs} =
    S.difference (M.keysSet objs) (mark S.empty (gcRoots st))
  where
    mark marked [] = marked
    mark marked (objRef:objRefs)
      | objRef `S.member` marked = mark marked objRefs
      | otherwise =
            mark (S.insert objRef marked) (gcObjRefs st objRef ++ objRefs)

gcDestruct :: [ObjRef] -> Op
gcDestruct [] cont st = cont st
gcDestruct (objRef:objRefs) cont st@State{stObjs = objs} =
    runFunc cclass "d__" objRef (gcDestruct objRefs cont) st
  where Obj{objClass = cclass} = objs M.! objRef

gcSweep :: Set ObjRef -> Op
gcSweep garbage cont st@State{stObjs = objs} =
    cont st{stObjs = M.filterWithKey (const . notGarbage) objs}
  where
    notGarbage objRef = -- including those resurrected by destructors
        not (objRef `S.member` (S.difference garbage (gcGarbage st)))



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

instance Show Val where
    show (ValGlobalName n) = "(" ++ n ++ ")"
    show (ValInstanceName n) = "(" ++ n ++ ")"
    show (ValLocalName n) = "(" ++ n ++ ")"
    show (ValString s) = show s
    show (ValNum n) = "<" ++ show n ++ ">"
    show (ValObj (ObjRef n)) = "[" ++ show n ++ "]"
    show (ValFunc _) = "[func]"
    show (ValClass _) = "[class]"

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

nameVal :: String -> Val
nameVal name@(ch:_)
  | isUpper ch = ValGlobalName name
  | isLower ch = ValInstanceName name
  | ch == '_' = ValLocalName name

boolVal :: Bool -> Val
boolVal b | b = ValNum 1 | otherwise = ValNum 0

testVal :: Val -> Bool
testVal (ValString (_:_)) = True
testVal (ValNum 0) = False
testVal (ValNum _) = True
testVal _ = False

getVar :: State -> Val -> Val
getVar State{stGlobals = globals} (ValGlobalName name) = globals M.! name
getVar State{stCallStack = CallFrame{cfObj = objRef}:_, stObjs = objs} (ValInstanceName name) =
    instanceVars M.! name
  where Obj{objVars = instanceVars} = objs M.! objRef
getVar State{stCallStack = CallFrame{cfLocals = locals}:_} (ValLocalName name) =
    locals M.! name

setVar :: State -> Val -> Val -> State
setVar st@State{stGlobals = globals} (ValGlobalName name) val =
    st{stGlobals = M.insert name val globals}
setVar st@State{stCallStack = CallFrame{cfObj = objRef}:_, stObjs = objs} (ValInstanceName name) val =
    st{stObjs = M.insert objRef obj{objVars = M.insert name val instanceVars} objs}
  where obj@Obj{objVars = instanceVars} = objs M.! objRef
setVar st@State{stCallStack = frame@CallFrame{cfLocals = locals}:frames} (ValLocalName name) val =
    st{stCallStack = frame{cfLocals = M.insert name val locals}:frames}

newObj :: CClass -> State -> (ObjRef,State)
newObj cclass st@State{stObjs = objs} =
    (objRef,st{stObjs = M.insert objRef Obj{objClass = cclass, objVars = M.empty} objs})
  where objRef | M.null objs = ObjRef 0
               | otherwise = ObjRef (n+1)
        (ObjRef n,_) = M.findMax objs

runFunc :: CClass -> String -> ObjRef -> Op
runFunc cclass func objRef cont st@State{stCallStack = frames}
  | not (M.member func cclass) = cont st
  | otherwise = runOp st{stCallStack = [newFrame]}
  where
    newFrame = CallFrame{cfOps = cclass M.! func, cfPC = 0, cfObj = objRef, cfLocals = M.empty}
    runOp st@State{stCallStack = []} = cont st{stCallStack = frames}
    runOp st@State{stCallStack = CallFrame{cfOps = ops, cfPC = pc}:_} =
        (ops A.! pc) runOp st

runMain :: State -> String -> String
runMain st@State{stGCFlag = flag, stGlobals = globals, stObjs = objs} =
    runFunc mainClass "c__" mainObj cont st'
  where
    ValClass mainClass = globals M.! "M"
    (mainObj,st') = newObj mainClass st{stGCFlag = True}
    cont = (setGCFlag flag . runFunc mainClass "m" mainObj . const . const) ""



--

initialState :: String -> State
initialState prog = State{
    stGlobals = (M.map ValClass . compile . (builtins ++) . parse . tokenize) prog,
    stStack = [],
    stCallStack = [],
    stObjs = M.empty,
    stGCFlag = False
    }

glass :: String -> IO ()
glass prog = (interact . runMain . initialState) prog
