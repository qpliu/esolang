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
     "declare fastcc void @.match_failure(i8*) noreturn",
     "declare fastcc void @.print_debug_memory()"]

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

isParamBound :: Param -> Bool
isParamBound (ParamBound _) = True
isParamBound _ = False

compileArities :: Map.Map String [Defn] -> [String]
compileArities fns = concatMap compileArity arities
  where
    arities :: [Int]
    arities = Set.toList (Map.fold addArity Set.empty fns)
    addArity :: [Defn] -> Set.Set Int -> Set.Set Int
    addArity defns@(Defn defnParams _:_) set =
        foldl addDefnArity (Set.insert (length defnParams) set) defns
    addDefnArity :: Set.Set Int -> Defn -> Set.Set Int
    addDefnArity set defn = Set.insert (defnArity defn) set
    defnArity :: Defn -> Int
    defnArity (Defn defnParams _) = length (filter isParamBound defnParams)
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
compileFn name defns@(Defn defnParams _:_) =
    fnNameConstant ++ fnPromise ++ fnEval
    ++ concat (zipWith (compileDefn name) [0..] defns)
  where
    fnNameConstant =
        [fnName ".name" name ++ " = private constant ["
             ++ show (length name + 1) ++ " x i8] c\""
             ++ mangleName name ++ "\\00\""]
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
             ++ "([" ++ show arity ++ " x %.val*]* %env) {",
         "    br label %p0"]
        ++ concat (zipWith evalPatternMatch [0..] defns)
        ++ ["  p" ++ show (length defns) ++ ":",
            "    call fastcc void @.match_failure(i8* getelementptr (["
                 ++ show (length name + 1) ++ " x i8]* "
                 ++ fnName ".name" name ++ ", i32 0, i32 0))",
            "    unreachable",
            "}"]
    evalPatternMatch index (Defn defnParams _) =
        ["  p" ++ show index ++ ":"]
        ++ concat (zipWith (evalMatchParam index) [0..] defnParams)
        ++ ["    %v" ++ show index ++ " = call fastcc %.val* "
                ++ fnName "" (defnName name index) ++ "("
                ++ drop 1 (concatMap (((",%.val* %b" ++ show index ++ ".") ++) . show) (boundParamIndices defnParams)) ++ ")",
            "    %r" ++ show index
                ++ " = tail call fastcc { i1, %.val* } @.eval(%.val* %v"
                ++ show index ++ ")",
            "    ret { i1, %.val* } %r" ++ show index]
    evalMatchParam index paramIndex (ParamBound bits) =
        evalMatchParamBits index paramIndex bits
        ++ ["    %b" ++ show index ++ "." ++ show paramIndex
                ++ " = bitcast %.val* %"
                ++ bitSymbol index paramIndex (length bits) ""
                ++ " to %.val*"]
    evalMatchParam index paramIndex (ParamLiteral bits) =
        evalMatchParamBits index paramIndex bits
        ++ ["    %" ++ bitSymbol index paramIndex (length bits) ".eval"
                ++ " = call fastcc { i1, %.val* } @.eval(%.val* %"
                ++ bitSymbol index paramIndex (length bits) ""
                ++ ")",
            "    %" ++ bitSymbol index paramIndex (length bits + 1) ""
                ++ " = extractvalue { i1, %.val* } %"
                ++ bitSymbol index paramIndex (length bits) ".eval" ++ ", 1",
            "    %" ++ bitSymbol index paramIndex (length bits) ".is_nil"
                ++ " = icmp eq %.val* null, %"
                ++ bitSymbol index paramIndex (length bits + 1) "",
            "    br i1 %" ++ bitSymbol index paramIndex (length bits) ".is_nil"
                ++ ", label %"
                ++ bitSymbol index paramIndex (length bits) ".match"
                ++ ", label %p" ++ show (index + 1),
            "  " ++ bitSymbol index paramIndex (length bits) ".match" ++ ":"]
    evalMatchParam index paramIndex (ParamWild bits) =
        evalMatchParamBits index paramIndex bits
    evalMatchParamBits index paramIndex bits =
        ["    %_" ++ bitSymbol index paramIndex 0 ""
             ++ " = getelementptr [" ++ show arity
             ++ " x %.val*]* %env, i32 0, i32 " ++ show paramIndex,
         "    %" ++ bitSymbol index paramIndex 0 ""
             ++ " = load %.val** %_" ++ bitSymbol index paramIndex 0 ""]
        ++ concat (zipWith (evalMatchParamBit index paramIndex) [0..] bits)
    evalMatchParamBit index paramIndex bitIndex bit =
        ["    %" ++ bitSymbol index paramIndex bitIndex ".eval"
             ++ " = call fastcc { i1, %.val* } @.eval(%.val* %"
             ++ bitSymbol index paramIndex bitIndex ""
             ++ ")",
         "    %" ++ bitSymbol index paramIndex (bitIndex + 1) ""
             ++ " = extractvalue { i1, %.val* } %"
             ++ bitSymbol index paramIndex bitIndex ".eval" ++ ", 1",
         "    %" ++ bitSymbol index paramIndex bitIndex ".is_nil"
             ++ " = icmp eq %.val* null, %"
             ++ bitSymbol index paramIndex (bitIndex + 1) "",
         "    br i1 %" ++ bitSymbol index paramIndex bitIndex ".is_nil"
             ++ ", label %p" ++ show (index + 1) ++ ", label %"
             ++ bitSymbol index paramIndex bitIndex ".not_nil",
         "  " ++ bitSymbol index paramIndex bitIndex ".not_nil:",
         "    %" ++ bitSymbol index paramIndex bitIndex ".bit"
             ++ " = extractvalue { i1, %.val* } %"
             ++ bitSymbol index paramIndex bitIndex ".eval" ++ ", 0",
         let matchLabel = bitSymbol index paramIndex bitIndex ".match"
             notMatchLabel = "p" ++ show (index + 1)
         in  "    br i1 %" ++ bitSymbol index paramIndex bitIndex ".bit"
                 ++ ", label %" ++ (if bit then matchLabel else notMatchLabel)
                 ++ ", label %" ++ (if bit then notMatchLabel else matchLabel),
         "  " ++ bitSymbol index paramIndex bitIndex ".match:"]
    bitSymbol index paramIndex bitIndex code =
        "b" ++ show index ++ "." ++ show paramIndex
             ++ "." ++ show bitIndex ++ code
    boundParamIndices defnParams =
        map snd (filter (isParamBound . fst) (zip defnParams [0..]))

compileDefn :: String -> Int -> Defn -> [String]
compileDefn name index (Defn defnParams defnExprs) =
    defnPromise ++ defnEval
  where
    arity = length (filter isParamBound defnParams)
    defnPromise =
        ["define private fastcc %.val* " ++ fnName "" (defnName name index)
             ++ "(" ++ drop 1 (concatMap ((",%.val* %a" ++) . show) [1..arity])
             ++ ") {",
         "    %val = tail call fastcc %.val* @.funcall" ++ show arity
                ++ "val({i1,%.val*}([" ++ show arity ++ " x %.val*]*)* "
                ++ fnName ".eval" (defnName name index)
                ++ concatMap ((",%.val* %a" ++) . show) [1..arity]
                ++ ")",
         "    ret %.val* %val",
         "}"]
    defnEval =
        ["define private fastcc { i1, %.val* } "
             ++ fnName ".eval" (defnName name index)
             ++ "([" ++ show arity ++ " x %.val*]* %env) {",
         "    ret { i1, %.val* } undef",
         "}"]

fnName :: String -> String -> String
fnName section name = "@\"01_" ++ mangleName name ++ section ++ "\""

defnName :: String -> Int -> String
defnName name index = name ++ '_' : show index

mangleName :: String -> String
mangleName name = foldr mangle "" name
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
