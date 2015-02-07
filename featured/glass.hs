-- http://esolangs.org/wiki/Glass

import Data.Array(Array,array,bounds,inRange)
import Data.Char(isAlpha,isDigit,isLower,isSpace,isUpper)
import Data.List(nub)
import Data.Map(Map,elems,fromList,(!))



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
cmdPushName = undefined

cmdDup :: Int -> Op
cmdDup = undefined

cmdPushStr :: String -> Op
cmdPushStr = undefined

cmdPushNum :: Float -> Op
cmdPushNum = undefined

cmdPop :: Op
cmdPop = undefined

cmdRet :: Op
cmdRet = undefined

cmdAssign :: Op
cmdAssign = undefined

cmdNew :: Op
cmdNew = undefined

cmdLookupFunc :: Op
cmdLookupFunc = undefined

cmdCallFunc :: Op
cmdCallFunc = undefined

cmdLookupVar :: Op
cmdLookupVar = undefined

cmdAssignObj :: Op
cmdAssignObj = undefined

cmdJump :: Int -> Op
cmdJump = undefined

cmdIfTrue :: String -> Int -> Op
cmdIfTrue = undefined

incPC :: Op
incPC cont st@State{stCallStack = fr@CallFrame{cfOps = ops, cfPC = pc}:frs} inp
  | bounds ops `inRange` (pc + 1) =
        cont st{stCallStack = fr{cfPC = pc+1}:frs} inp
  | otherwise = cmdRet cont st inp



-- builtins

builtins :: [Class]
builtins = [
    Class "A" [
        FunctionBuiltin "a" undefined,
        FunctionBuiltin "s" undefined,
        FunctionBuiltin "m" undefined,
        FunctionBuiltin "d" undefined,
        FunctionBuiltin "mod" undefined,
        FunctionBuiltin "f" undefined,
        FunctionBuiltin "e" undefined,
        FunctionBuiltin "ne" undefined,
        FunctionBuiltin "lt" undefined,
        FunctionBuiltin "le" undefined,
        FunctionBuiltin "gt" undefined,
        FunctionBuiltin "ge" undefined
        ],
    Class "S" [
        FunctionBuiltin "l" undefined,
        FunctionBuiltin "i" undefined,
        FunctionBuiltin "si" undefined,
        FunctionBuiltin "a" undefined,
        FunctionBuiltin "d" undefined,
        FunctionBuiltin "e" undefined,
        FunctionBuiltin "ns" undefined,
        FunctionBuiltin "sn" undefined
        ],
    Class "V" [
        FunctionBuiltin "n" undefined,
        FunctionBuiltin "d" undefined
        ],
    Class "O" [
        FunctionBuiltin "o" undefined,
        FunctionBuiltin "on" undefined
        ],
    Class "I" [
        FunctionBuiltin "l" undefined,
        FunctionBuiltin "c" undefined,
        FunctionBuiltin "e" undefined
        ]
    ]



-- garbage collector

gc :: Op
gc cont st@State{stGCFlag = True} inp = cont st inp -- running destructors
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
    ValName String
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
