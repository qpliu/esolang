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
resolvePartial arity (PartialDef (Identifier pos name) params unparsed) = do
    undefined
