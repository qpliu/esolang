module Nodes(Nodes,Node,new,newNode,hasEdge,addEdge,removeEdge,edges,gc)
where

import qualified Data.Map as Map
import qualified Data.Set as Set

newtype Nodes = Nodes (Map.Map Node (Set.Set Node)) deriving Show

newtype Node = Node Integer deriving (Eq,Ord,Show)

new :: Nodes
new = Nodes Map.empty

newNode :: Nodes -> (Nodes,Node)
newNode (Nodes nodes) = (Nodes (Map.insert node Set.empty nodes), node)
  where node | nodes == Map.empty = Node 1
             | otherwise = let (Node n,_) = Map.findMax nodes in Node (n+1)

hasEdge :: Nodes -> Node -> Node -> Bool
hasEdge (Nodes nodes) n1 n2 = Set.member n2 (nodes Map.! n1)

addEdge :: Nodes -> Node -> Node -> Nodes
addEdge (Nodes nodes) n1 n2 = Nodes (Map.adjust (Set.insert n2) n1 nodes)

removeEdge :: Nodes -> Node -> Node -> Nodes
removeEdge (Nodes nodes) n1 n2 = Nodes (Map.adjust (Set.delete n2) n1 nodes)

edges :: Nodes -> Node -> [Node]
edges (Nodes nodes) node = Set.toList (nodes Map.! node)

gc :: Nodes -> [Node] -> Nodes
gc (Nodes nodes) rootSet = Nodes (copyCollector rootSet Map.empty)
  where
    copyCollector [] newNodes = newNodes
    copyCollector (n:ns) newNodes
      | Map.member n newNodes = copyCollector ns newNodes
      | otherwise = copyCollector (Set.toList edges ++ ns)
                                  (Map.insert n edges newNodes)
      where
        edges = nodes Map.! n
