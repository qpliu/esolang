module Resolve
    (resolve)
where

import Control.Monad(foldM)
import Data.Map(Map)
import qualified Data.Map as Map

import Ast
    (Identifier(..),PartialDef(..),Param(..),Unparsed(..),
     Def(..),Expr(..),Func)
import Compile(Compile,SourcePos,compileError)

resolve :: [PartialDef] -> Compile [Func]
resolve partialDefs = do
    arities <- foldM checkArity Map.empty partialDefs
    fmap Map.toList (foldM (collectDef (flip Map.lookup arities))
                           (Map.map (const []) arities) partialDefs)

checkArity :: Map String Int -> PartialDef -> Compile (Map String Int)
checkArity arities (PartialDef (Identifier pos name) params _)
  | not (Map.member name arities) =
        return (Map.insert name (length params) arities)
  | length params == arities Map.! name = return arities
  | otherwise =
        compileError pos (name ++ " previously defined with "
                               ++ show (arities Map.! name)
                               ++ " parameter(s), redefined with "
                               ++ show (length params) ++ " parameter(s)")

collectDef :: (String -> Maybe Int) -> Map String [Def] -> PartialDef
           -> Compile (Map String [Def])
collectDef arity defs partialDef@(PartialDef (Identifier _ name) _ _) = do
    def <- resolvePartial arity partialDef
    return (Map.adjust (++ [def]) name defs)

resolvePartial :: (String -> Maybe Int) -> PartialDef -> Compile Def
resolvePartial arity (PartialDef (Identifier pos name) params unparsed)
  | null unparsed = return (Def params (ExprLiteral pos []))
  | otherwise = resolveBody arity params unparsed >>= return . Def params

resolveBody :: (String -> Maybe Int) -> [Param] -> [Unparsed] -> Compile Expr
resolveBody arity params unparsed = do
    (expr,rest) <- resolveExpr arity params unparsed
    if null rest
        then return expr
        else do
            expr2 <- resolveBody arity params rest
            return (ExprConcat expr expr2)

resolveExpr :: (String -> Maybe Int) -> [Param] -> [Unparsed]
            -> Compile (Expr,[Unparsed])
resolveExpr arity params (unparsed:rest) = resolve1 unparsed
  where
    resolve1 (UnparsedLiteral pos bits) = return (ExprLiteral pos bits,rest)
    resolve1 (UnparsedIdentifier ident@(Identifier pos name)) =
        maybe (resolveFuncall ident) return (resolveBound 0 params name)

    resolveBound index [] name = Nothing
    resolveBound index (ParamBound _ _ (Identifier _ paramName):params) name
      | name == paramName = Just (ExprBound index,rest)
      | otherwise = resolveBound (index+1) params name
    resolveBound index (_:params) name = resolveBound index params name

    resolveFuncall ident@(Identifier pos name) =
        maybe (compileError pos ("Undefined symbol '" ++ name ++ "'"))
              (resolveFuncallArgs ident rest []) (arity name)

    resolveFuncallArgs ident@(Identifier pos name) rest args nargs
      | length args == nargs = return (ExprFuncall ident (reverse args),rest)
      | null rest = compileError pos ("Function '" ++ name ++ "' takes "
                                                   ++ (show nargs)
                                                   ++ " argument(s), given "
                                                   ++ (show (length args))
                                                   ++ " argument(s)")
      | otherwise = do
            (arg,rest) <- resolveExpr arity params rest
            resolveFuncallArgs ident rest (arg:args) nargs
