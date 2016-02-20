module Resolve
    (resolve)
where

import Control.Monad(foldM)
import Data.Char(isDigit)
import Data.Map(Map)
import qualified Data.Map as Map

import Ast(Identifier(..),PartialDef(..),Token(..),Def(..),Expr(..))
import Compile(Compile,SourcePos,compileError)

resolve :: [PartialDef] -> Compile [Def]
resolve partialDefs = do
    arities <- foldM resolveArity Map.empty partialDefs
    halfResolved <- mapM (halfResolve (flip Map.lookup arities)) partialDefs
    return (Map.elems (fullyResolve halfResolved))

resolveArity :: Map String Int -> PartialDef -> Compile (Map String Int)
resolveArity arities (PartialDef (Identifier pos name) params _ _)
  | Map.member name arities =
        compileError pos ("multiple definitions of " ++ name)
  | otherwise = return (Map.insert name (length params) arities)

data HalfResolved =
    HalfResolvedBag SourcePos [(Int,HalfResolved)]
  | HalfResolvedBinop SourcePos String HalfResolved HalfResolved
  | HalfResolvedBound SourcePos String Int
  | HalfResolvedFuncall SourcePos String [(Bool,HalfResolved)]

halfResolve :: (String -> Maybe Int) -> PartialDef
            -> Compile (PartialDef,HalfResolved)
halfResolve arity partialDef@(PartialDef _ params tokens defEndPos) = do
    paramIndex <- resolveParams params
    halfResolved <- halfResolveExpr defEndPos arity paramIndex tokens
    return (partialDef,halfResolved)

resolveParams :: [Identifier] -> Compile (String -> Maybe Int)
resolveParams params = do
    paramMap <- foldM resolveParam Map.empty (zip params [0..])
    return (flip Map.lookup paramMap)
  where
    resolveParam paramMap ((Identifier pos name),index)
      | Map.member name paramMap =
            compileError pos ("duplicate parameter name " ++ name)
      | otherwise = return (Map.insert name index paramMap)

halfResolveExpr :: SourcePos -> (String -> Maybe Int) -> (String -> Maybe Int)
                -> [Token]
                -> Compile HalfResolved
halfResolveExpr defEndPos arity paramIndex tokens = do
    (expr,toks) <- r tokens
    case toks of
        [] -> return expr
        (Token pos _:_) -> compileError pos "unexpected token"
  where
    r [] = compileError defEndPos "unexpected end of definition body"
    r tokens = do
        (lhs,toks) <- r1 tokens
        rbinop lhs toks
    r1 (Token pos name:toks)
      | name == "(" && not (null toks) = do
            (expr,toks) <- r toks
            case toks of
                (Token _ ")":toks) -> return (expr,toks)
                _ -> compileError pos "unmatched ("
      | name == "[" && not (null toks) = do
            (exprs,toks) <- rbagElements pos [] toks
            return (HalfResolvedBag pos exprs,toks)
      | maybe False (const True) (paramIndex name) =
            let Just index = paramIndex name
            in  return (HalfResolvedBound pos name index,toks)
      | maybe False (const True) (arity name) =
            let Just nargs = arity name
            in  do
                    (args,toks) <- foldM rfuncallArg ([],toks) [1..nargs]
                    return (HalfResolvedFuncall pos name (reverse args),toks)
      | otherwise = compileError pos ("unexpected token " ++ name)
    rbinop lhs toks@(Token pos tok:rhsToks)
      | elem tok ["|","∪","&","∩","^","△","⊖"] = do
            (rhs,toks) <- r1 rhsToks
            rbinop (HalfResolvedBinop pos tok lhs rhs) toks
    rbinop lhs toks = return (lhs,toks)
    rbagElements startPos exprs [] = compileError startPos "unmatched ["
    rbagElements startPos exprs (Token _ "]":toks) =
        return (reverse exprs,toks)
    rbagElements startPos exprs toks = do
        (count,toks) <- rcount toks
        (expr,toks) <- r toks
        rbagElements startPos ((count,expr):exprs) toks
    rcount (Token _ count:Token _ timesTok:toks)
      | all isDigit count && (timesTok == "*" || timesTok == "×") =
            return (read count,toks)
    rcount toks = return (1,toks)
    rfuncallArg (args,toks) _ = do
        (isMap,toks) <- rmap toks
        (expr,toks) <- r1 toks
        return ((isMap,expr):args,toks)
    rmap (Token _ "*":toks) = return (True,toks)
    rmap toks = return (False,toks)

fullyResolve :: [(PartialDef,HalfResolved)] -> Map String Def
fullyResolve halfResolved = defs
  where
    defs = Map.fromList (map r halfResolved)
    r (PartialDef identifier@(Identifier _ name) params _ _,halfResolvedExpr) =
        (name,Def identifier (length params) (rexpr halfResolvedExpr))
    rexpr (HalfResolvedBag pos elts) = ExprBag pos (map (fmap rexpr) elts)
    rexpr (HalfResolvedBinop pos op lhs rhs) =
        (exprBinop op) pos (rexpr lhs) (rexpr rhs)
    rexpr (HalfResolvedBound pos name index) = ExprBound pos name index
    rexpr (HalfResolvedFuncall pos name args) =
        let Just func = Map.lookup name defs
        in  ExprFuncall pos func (map (fmap rexpr) args)
    exprBinop "|" = ExprUnion
    exprBinop "∪" = ExprUnion
    exprBinop "&" = ExprIntersect
    exprBinop "∩" = ExprIntersect
    exprBinop "^" = ExprDiff
    exprBinop "△" = ExprDiff
    exprBinop "⊖" = ExprDiff
