module Compiler(compile) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Parser
    (Defn(Defn),
     Expr(ExprLiteral,ExprParam,ExprFuncall),
     Param(ParamBound,ParamLiteral,ParamWild),
     arity)

compile :: String -> Bool -> Map.Map String [Defn] -> String
compile mainFunction debug fns =
    maybe (error ("No such function: " ++ mainFunction))
          (unlines . compileProgram) (arity fns mainFunction)
  where
    compileProgram :: Int -> [String]
    compileProgram mainArity =
          runtime
       ++ compileConstants fns
       ++ compileArities fns
       ++ compileFns fns
       ++ compileMain mainFunction mainArity debug

runtime :: [String]
runtime =
    ["declare fastcc void @.free(i8*)",
     "declare fastcc i8* @.alloc(i32)",
     "%.val = type { i32, i1, %.val*, { i1, %.val* } (i8*)*, void (i8*)*, i8* }",
     "@.nil = external global %.val",
     "declare fastcc %.val* @.addref(%.val*)",
     "declare fastcc void @.deref(%.val*)",
     "declare fastcc %.val* @.newval({i1,%.val*} (i8*)*, void (i8*)*, i8*)",
     "declare fastcc { i1, %.val* } @.eval(%.val*)",
     "declare fastcc %.val* @.literalval([0 x i1]*, i32, i32)",
     "declare fastcc %.val* @.fileval(i32, i8, i8)",
     "declare fastcc %.val* @.unopenedfileval(i8*)",
     "declare fastcc %.val* @.concatval(%.val*, %.val*)",
     "declare fastcc void @.print(%.val*)",
     "declare fastcc %.val* @.getarg(i32, %.val*, i32, i8**)",
     "declare fastcc void @.print_debug_memory()",
     ""]

constantName :: [Bool] -> String
constantName bits = "@L" ++ map (\ b -> if b then '1' else '0') bits

compileConstants :: Map.Map String [Defn] -> [String]
compileConstants fns = map compileConstant constants
  where
    compileConstant :: [Bool] -> String
    compileConstant bits =
        constantName bits ++ " = linkonce_odr constant ["
            ++ show (length bits) ++ " x i1] ["
            ++ drop 1 (concatMap compileConstantBit bits)
            ++ "]"
    compileConstantBit b = ",i1 " ++ if b then "1" else "0"
    constants :: [[Bool]]
    constants =
        Set.toList (Map.fold (flip (foldl defnConstants)) Set.empty fns)
    defnConstants :: Set.Set [Bool] -> Defn -> Set.Set [Bool]
    defnConstants set (Defn _ exprs) = foldl exprConstants set exprs
    exprConstants :: Set.Set [Bool] -> Expr -> Set.Set [Bool]
    exprConstants set (ExprLiteral bits) = Set.insert bits set
    exprConstants set (ExprParam _) = set
    exprConstants set (ExprFuncall _ exprs) = foldl exprConstants set exprs

compileArities :: Map.Map String [Defn] -> [String]
compileArities fns = concatMap compileArity arities
  where
    arities :: [Int]
    arities = Set.toList (Map.fold addArity Set.empty fns)
    addArity :: [Defn] -> Set.Set Int -> Set.Set Int
    addArity (Defn defnParams _:_) set = Set.insert (length defnParams) set
    compileArity :: Int -> [String]
    compileArity arity =
        ["define private %.val* @.funcall" ++ show arity
             ++ "val({ i1, %.val* } ([" ++ show arity ++ " x %.val*]*)* %eval"
             ++ concatMap ((",%.val* %a" ++) . show) [1..arity]
             ++ ") {"]
        ++ compileConstructEnv arity
        ++ ["    %eval_for_newval = bitcast {i1,%.val*}([" ++ show arity ++ " x %.val*]*)* %eval to {i1,%.val*}(i8*)*",
            "    %freeenv = bitcast void ([" ++ show arity ++ " x %.val*]*)* @.funcall" ++ show arity ++ "val.freeenv to void (i8*)*",
            "    %val = tail call fastcc %.val* @.newval({i1,%.val*}(i8*)* %eval_for_newval, void (i8*)* %freeenv, i8* %env_from_alloc)",
            "    ret %.val* %val",
            "}",
            "define private void @.funcall" ++ show arity
                ++ "val.freeenv([" ++ show arity ++ " x %.val*]* %env) {"]
        ++ concatMap (compileDerefEnvVal arity) [1..arity]
        ++ (if arity == 0
                then []
                else ["    %env_for_free = bitcast [" ++ show arity
                          ++ " x %.val*]* %env to i8*",
                      "    tail call fastcc void @.free(i8* %env_for_free)"])
        ++ ["    ret void",
            "}"]
    compileConstructEnv :: Int -> [String]
    compileConstructEnv 0 = ["    %env_from_alloc = bitcast i8* undef to i8*"]
    compileConstructEnv arity =
        ["    %env_size = ptrtoint %.val* getelementptr (%.val* null, i32 " ++ show arity ++ ") to i32",
         "    %env_from_alloc = call fastcc i8* @.alloc(i32 %env_size)",
         "    %env = bitcast i8* %env_from_alloc to ["
            ++ show arity ++ " x %.val*]*"]
        ++ concatMap (compileAddParamToEnv arity) [1..arity]
    compileAddParamToEnv :: Int -> Int -> [String]
    compileAddParamToEnv arity index =
        ["    call fastcc %.val* @.addref(%.val* %a" ++ show index ++ ")",
         "    %_env" ++ show index ++ " = getelementptr [" ++ show arity ++ " x %.val*]* %env, i32 0, i32 " ++ show (index - 1),
         "    store %.val* %a" ++ show index ++ ", %.val** %_env" ++ show index]
    compileDerefEnvVal :: Int -> Int -> [String]
    compileDerefEnvVal arity index =
        ["    %_env" ++ show index ++ " = getelementptr [" ++ show arity ++ " x %.val*]* %env, i32 0, i32 " ++ show (index - 1),
         "    %a" ++ show index ++ " = load %.val** %_env" ++ show index,
         "    call fastcc void @.deref(%.val* %a" ++ show index ++ ")"]

compileFns :: Map.Map String [Defn] -> [String]
compileFns fns = concatMap (uncurry compileFn) (Map.assocs fns)

compileFn :: String -> [Defn] -> [String]
compileFn name defns@(Defn defnParams _:_) = fnPromise ++ fnEval ++ fnFreeEnv
  where
    arity = length defnParams
    fnPromise =
        ["define fastcc %.val* " ++ fnName "" name ++ "("
            ++ drop 1 (concatMap ((",%.val* %a" ++) . show) [1..arity])
            ++ ") {",
         "    %val = tail call fastcc %.val* @.funcall" ++ show arity
                ++ "val({i1,%.val*}([" ++ show arity ++ " x %.val*]*)* "
                ++ fnName ".eval" name
                ++ concatMap ((",%.val* %a" ++) . show) [1..arity]
                ++ ")",
         "    ret %.val* %val",
         "}"]
    fnEval =
        ["define private fastcc { i1, %.val* } " ++ fnName ".eval" name
             ++ "([" ++ show arity ++ " x %.val*]* %env) {"]
        ++ ["    ret { i1, %.val* } undef",
            "}"]
    fnFreeEnv =
        ["define private fastcc void " ++ fnName ".freeenv" name
             ++ "([" ++ show arity ++ " x %.val*]* %env) {",
         ""]
        ++ (if arity == 0
                then []
                else [
                      ])
        ++ ["    ret void",
            "}"]
    compileBody :: String -> Int -> Int -> [Expr] -> [String]
    compileBody name sequenceNumber nparams exprs = []

fnName :: String -> String -> String
fnName section name = "@\"01_" ++ foldr mangle "" name ++ section ++ "\""
  where
    mangle '"' str = "\\22" ++ str
    mangle '\\' str = "\\5C" ++ str
    mangle c str = c:str

compileMain :: String -> Int -> Bool -> [String]
compileMain mainFunction mainArity debug =
    ["define void @main(i32 %argc, i8** %argv) {",
     "    %stdin = call fastcc %.val* @.fileval(i32 0, i8 undef, i8 7)"]
    ++ map getArg [1..mainArity]
    ++ ["    %val = call fastcc %.val* "
            ++ fnName "" mainFunction ++ "("
            ++ drop 1 (concatMap argParam [1..mainArity]) ++ ")",
        "    call fastcc void @.deref(%.val* %stdin)"]
    ++ map derefArg [1..mainArity]
    ++ ["    tail call fastcc void @.print(%.val* %val)"]
    ++ (if debug
            then ["    tail call fastcc void @.print_debug_memory()"]
            else [])
    ++ ["    ret void",
        "}"]
  where
    getArg index =
        "    %a" ++ show index ++ " = call fastcc %.val* @.getarg(i32 "
            ++ show index ++ ", %.val* %stdin, i32 %argc, i8** %argv)"
    derefArg index =
        "    call fastcc void @.deref(%.val* %a" ++ show index ++ ")"
    argParam index = ",%.val* %a" ++ show index
