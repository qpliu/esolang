module Scope
    (Scope,Var,
     new,push,pop,get,set,eq,hasEdge,addEdge,removeEdge,
     edges,pushDoEdges,popDoEdges,gc)
where

import qualified Data.Map as Map
import qualified Vars
import Vars(Var)

newtype Scope = Scope (Vars.Vars,[Map.Map String Var],[[Var]])

new :: Scope
new = Scope (Vars.new,[Map.empty],[])

push :: Scope -> Scope
push (Scope (vars,scopes,doEdges)) = Scope (vars,Map.empty:scopes,doEdges)

pop :: Scope -> Scope
pop (Scope (vars,_:scopes,doEdges)) = Scope (vars,scopes,doEdges)

get :: Scope -> String -> (Var,Scope)
get s@(Scope (vars,scope:scopes,doEdges)) var
  | var == "0" =
        let (newVars,newVar) = Vars.newVar vars
        in  (newVar,Scope (newVars,scope:scopes,doEdges))
  | Map.member var scope = (scope Map.! var,s)
  | otherwise =
        let (newVars,newVar) = Vars.newVar vars
            newScope = Map.insert var newVar scope
        in  (newVar,Scope (newVars,newScope:scopes,doEdges))

set :: Scope -> String -> Var -> Scope
set s v1 v2 =
    let (var1,Scope (vars,scopes,doEdges)) = get s v1
    in  Scope (Vars.set vars var1 v2,scopes,doEdges)

eq :: Scope -> Var -> Var -> Bool
eq (Scope (vars,_,_)) v1 v2 = Vars.eq vars v1 v2

hasEdge :: Scope -> Var -> Var -> Bool
hasEdge (Scope (vars,_,_)) v1 v2 = Vars.hasEdge vars v1 v2

addEdge :: Scope -> Var -> Var -> Scope
addEdge (Scope (vars,scopes,doEdges)) v1 v2 =
    Scope (Vars.addEdge vars v1 v2,scopes,doEdges)

removeEdge :: Scope -> Var -> Var -> Scope
removeEdge (Scope (vars,scopes,doEdges)) v1 v2 =
    Scope (Vars.removeEdge vars v1 v2,scopes,doEdges)

edges :: Scope -> Var -> ([Var],Scope)
edges s@(Scope (vars,scopes,doEdges)) var =
    let (newVars,edgeVars) = Vars.edges vars var
    in  (edgeVars,Scope (newVars,scopes,doEdges))
    
pushDoEdges :: Scope -> [Var] -> Scope
pushDoEdges (Scope (vars,scopes,doEdges)) edges =
    Scope (vars,scopes,edges:doEdges)
    
popDoEdges :: Scope -> Scope
popDoEdges (Scope (vars,scopes,doEdges)) =
    Scope (vars,scopes,tail doEdges)

gc :: Scope -> Scope
gc (Scope (vars,scopes,doEdges)) =
    Scope (Vars.gc vars (concatMap Map.elems scopes ++ concat doEdges),scopes,doEdges)
