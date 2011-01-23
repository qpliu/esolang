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
          (unlines . compile') (arity fns mainFunction)
  where
    compile' :: Int -> [String]
    compile' mainArity =
          runtime
       ++ compileConstants fns
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
     "declare fastcc %.val* @.newval({i1,%.val*} (i8*), void (i8*)*, i8*)",
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
compileConstants fns = map compileConstant (constants fns)

compileConstant :: [Bool] -> String
compileConstant bits =
    constantName bits ++ " = linkonce_odr constant ["
        ++ show (length bits) ++ " x i1] ["
        ++ drop 1 (concatMap (\ b -> ",i1 " ++ if b then "1" else "0") bits)
        ++ "]"

constants :: Map.Map String [Defn] -> [[Bool]]
constants fns =
    Set.toList (Map.fold (flip (foldl defnConstants)) Set.empty fns)

defnConstants :: Set.Set [Bool] -> Defn -> Set.Set [Bool]
defnConstants set (Defn _ exprs) = foldl exprConstants set exprs

exprConstants :: Set.Set [Bool] -> Expr -> Set.Set [Bool]
exprConstants set (ExprLiteral bits) = Set.insert bits set
exprConstants set (ExprParam _) = set
exprConstants set (ExprFuncall _ exprs) = foldl exprConstants set exprs

compileFns :: Map.Map String [Defn] -> [String]
compileFns fns = concatMap (uncurry compileFn) (Map.assocs fns)

compileFn :: String -> [Defn] -> [String]
compileFn name defns@(Defn defnParams _:_) = fnPromise ++ fnEval ++ fnFreeEnv
  where
    arity = length defnParams
    fnPromise =
        ["define fastcc %.val* " ++ fnName "" name ++ "("
            ++ drop 1 (concatMap ((",%.val* %a" ++) . show) [1..arity])
            ++ ") {"]
        ++ (if arity == 0
                then ["    ret %.val* undef"]
                else fnPromiseBody)
        ++ ["}"]
    fnPromiseBody = []
    fnEval =
        ["define private fastcc { i1, %.val* } " ++ fnName ".eval" name
             ++ "([" ++ show arity ++ " x %.val*]* %env) {"]
        ++ ["    ret { i1, %.val* } null",
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

fnName :: String -> String -> String
fnName section name = "@\"01_" ++ foldr mangle "" name ++ section ++ "\""
  where
    mangle '"' str = "\\22" ++ str
    mangle '\\' str = "\\5C" ++ str
    mangle c str = c:str

compileBody :: String -> Int -> Int -> [Expr] -> [String]
compileBody name sequenceNumber nparams exprs = []

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
