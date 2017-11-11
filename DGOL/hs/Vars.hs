module Vars(Vars,Var,new,newVar,eq,hasEdge,addEdge,removeEdge,gc)
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Nodes

newtype Vars = Vars (Nodes.Nodes,Map.Map Var Nodes.Node)

newtype Var = Var Integer deriving (Eq,Ord)

new :: Vars
new = Vars (Nodes.new,Map.empty)

newVar :: Vars -> (Vars,Var)
newVar (Vars (nodes,vars)) = (Vars (newNodes,Map.insert var node vars),var)
  where
    (newNodes,node) = Nodes.newNode nodes
    var | vars == Map.empty = Var 1
        | otherwise = let (Var n,_) = Map.findMax vars in Var (n+1)

eq :: Vars -> Var -> Var -> Bool
eq (Vars (_,vars)) v1 v2 = vars Map.! v1 == vars Map.! v2

hasEdge :: Vars -> Var -> Var -> Bool
hasEdge (Vars (nodes,vars)) v1 v2 =
    Nodes.hasEdge nodes (vars Map.! v1) (vars Map.! v2)

addEdge :: Vars -> Var -> Var -> Vars
addEdge (Vars (nodes,vars)) v1 v2 = Vars (newNodes,vars)
  where
    newNodes = Nodes.addEdge nodes (vars Map.! v1) (vars Map.! v2)

removeEdge :: Vars -> Var -> Var -> Vars
removeEdge (Vars (nodes,vars)) v1 v2 = Vars (newNodes,vars)
  where
    newNodes = Nodes.removeEdge nodes (vars Map.! v1) (vars Map.! v2)

gc :: Vars -> [Var] -> Vars
gc (Vars (nodes,vars)) rootSet = Vars (newNodes,newVars)
  where
    newVars = Map.filterWithKey (const . flip Set.member (Set.fromList rootSet)) vars
    newNodes = Nodes.gc nodes (Map.elems newVars)
