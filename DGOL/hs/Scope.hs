module Scope(Scope,Var,new,push,pop,get,set,eq,hasEdge,addEdge,removeEdge,gc)
where

import qualified Data.Map as Map
import qualified Vars
import Vars(Var)

newtype Scope = Scope (Vars.Vars,[Map.Map String Vars.Var])

new :: Scope
new = Scope (Vars.new,[Map.empty])

push :: Scope -> Scope
push (Scope (vars,scopes)) = Scope (vars,Map.empty:scopes)

pop :: Scope -> Scope
pop (Scope (vars,_:scopes)) = Scope (vars,scopes)

get :: Scope -> String -> (Vars.Var,Scope)
get scope var = undefined

set :: Scope -> String -> String -> Scope
set scope v1 v2 = undefined

eq :: Scope -> Vars.Var -> Vars.Var -> Bool
eq scope v1 v2 = undefined

hasEdge :: Scope -> Vars.Var -> Vars.Var -> Bool
hasEdge scope v1 v2 = undefined

addEdge :: Scope -> Vars.Var -> Vars.Var -> Scope
addEdge scope v1 v2 = undefined

removeEdge :: Scope -> Vars.Var -> Vars.Var -> Scope
removeEdge scope v1 v2 = undefined

gc :: Scope -> Scope
gc (Scope (vars,scopes)) =
    Scope (Vars.gc vars (concatMap Map.elems scopes),scopes)
