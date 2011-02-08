module Compiler(compile) where

import qualified Data.Map as Map

import Flattener(Expr(Arg,Quote,CarFn,CdrFn,ConsFn,If,Call))
import Reader(Value(Cons,Nil))

compile :: (Map.Map Int (Value,Expr),Expr) -> String
compile (fns,expr) =
    let names index =
            "@\"" ++ show index ++ show (fst ((Map.!) fns index)) ++ "\""
        consts = constants fns expr
    in  unlines (runtime
                 ++ map (compileConstant consts) (Map.assocs consts)
                 ++ concatMap (compileFn names consts)
                              (Map.assocs (Map.map snd fns))
                 ++ compileMain names consts expr)

runtime :: [String]
runtime =
    ["%val = type { i32, %val*, %val*, %eval (i8*)*, void (i8*)*, i8* }",
     "%eval = type { %val*, %val* }",
     "declare fastcc %val* @"(addref)"(%val*)",
     "declare fastcc void @"(deref)"(%val*)",
     "declare fastcc %val* @"(car)"(%val*)",
     "declare fastcc %val* @"(cdr)"(%val*)",
     "declare fastcc %val* @"(cons)"(%val*,%val*)",
     "declare fastcc %val* @"(if)"(%val*,%val*,%val*)",
     "@C0 = external global %val"]

constants :: Map.Map Int (Value,Expr) -> Expr -> Map.Map Value Int
constants fns expr =
    Map.fold collectConstants
             (collectConstants expr (Map.insert Nil 0 Map.empty))
             (Map.map snd fns)
  where
    insertConstant map Nil = map
    insertConstant map value@(Cons head tail)
      | Map.member value map = map
      | otherwise = let map1 = insertConstant (insertConstant map head) tail
                    in  Map.insert value (Map.size map1) map1
    collectConstants :: Expr -> Map.Map Value Int -> Map.Map Value Int
    collectConstants Arg map = map
    collectConstants (Quote value) map = insertConstant map value
    collectConstants (CarFn expr) map = collectConstants expr map
    collectConstants (CdrFn expr) map = collectConstants expr map
    collectConstants (ConsFn expr1 expr2) map =
        collectConstants expr2 (collectConstants expr1 map)
    collectConstants (If expr1 expr2 expr3) map =
        collectConstants expr3
                         (collectConstants expr2 (collectConstants expr1 map))
    collectConstants (Call _ expr) map = collectConstants expr map

compileConstant :: Map.Map Value Int -> (Value,Int) -> String
compileConstant consts (Nil,index) = ""
compileConstant consts (Cons car cdr,index) =
    "@C" ++ show index ++ " = global %val { i32 1, "
         ++ "%val* @C" ++ show ((Map.!) consts car) ++ ", "
         ++ "%val* @C" ++ show ((Map.!) consts cdr) ++ ", "
         ++ "%eval (i8*)* null, void (i8*)* null, i8* null }"
         ++ " ; " ++ show (Cons car cdr)

compileFn :: (Int -> String) -> Map.Map Value Int -> (Int,Expr) -> [String]
compileFn names consts (index,expr) =
    compileFunc names consts (names index) expr

compileMain :: (Int -> String) -> Map.Map Value Int -> Expr -> [String]
compileMain names consts expr = compileFunc names consts "@\"(main)\"" expr

compileFunc :: (Int -> String) -> Map.Map Value Int -> String -> Expr
                               -> [String]
compileFunc names consts name expr = undefined
