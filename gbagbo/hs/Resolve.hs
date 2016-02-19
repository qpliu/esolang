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
    HalfResolvedBag [(Int,HalfResolved)]
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
    r1 (Token pos "(":toks@(_:_)) = do
        (expr,toks) <- r toks
        case toks of
            (Token _ ")":toks) -> return (expr,toks)
            _ -> compileError pos "unmatched ("
    r1 (Token pos name:toks)
      | name == "(" && not (null toks) = do
            (expr,toks) <- r toks
            case toks of
                (Token _ ")":toks) -> return (expr,toks)
                _ -> compileError pos "unmatched ("
      | name == "[" && not (null toks) = do
            (exprs,toks) <- rbagElements pos [] toks
            return (HalfResolvedBag exprs,toks)
      | maybe False (const True) (paramIndex name) =
            let Just index = paramIndex name
            in  return (HalfResolvedBound pos name index,toks)
      | maybe False (const True) (arity name) =
            let Just nargs = arity name
            in  do
                -- handle funcall arguments
                undefined
      | otherwise = compileError pos "unexpected token"
    rbinop lhs toks@(Token pos tok:rhsToks)
      | elem tok ["|","∪","&","∩","^","△","⊖"] = do
            (rhs,toks) <- r1 rhsToks
            rbinop (HalfResolvedBinop pos tok lhs rhs) toks
      | otherwise = return (lhs,toks)
    rbagElements startPos exprs toks = undefined
    rcount (Token _ count:Token _ timesTok:toks)
      | all isDigit count && (timesTok == "*" || timesTok == "×") =
            (read count,toks)
    rcount toks = (1,toks)
    rmap (Token _ "*":toks) = (True,toks)
    rmap toks = (False,toks)

fullyResolve :: [(PartialDef,HalfResolved)] -> Map String Def
fullyResolve halfResolved = undefined
