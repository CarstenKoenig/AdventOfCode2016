module Astar
  ( Parameter (..)
  , Path
  , aStar
  ) where

import Data.Foldable (minimumBy)
import Data.Function (on)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S


data Parameter node eqNode
  = Parameter { heuristic :: node -> Int
              , neighbours :: node -> [node]
              , isGoal :: node -> Bool
              , eqClass :: node -> eqNode
              }


type Path node = [node]


aStar :: (Ord eqNode, Ord node, Show node) =>
         Parameter node eqNode -> node -> Path node
aStar params start =
  let env =
        Env
          S.empty
          (S.singleton start)
          M.empty
          (initG start)
          (initF start)
          params
  in algorithm' env
  where
    initG node =
      M.insert node 0 M.empty
    initF node =
      M.insert node (heuristic params node) M.empty


data Environment node eqNode
  = Env { closed :: Set eqNode
        , open :: Set node
        , cameFrom :: Map node node
        , gScores :: Map node Int
        , fScores :: Map node Int
        , params :: Parameter node eqNode
        }


algorithm' :: (Ord eqNode, Ord node, Show node) =>
              Environment node eqNode -> Path node
algorithm' env =
  let current = findCurrent env
  in case current of
    Nothing -> []
    Just current
      | finished env current ->
        constructPath env current
      | otherwise ->
        let open' = S.delete current $ open env
            closed' = S.insert (getClass env current) $ closed env
            env' = env { closed = closed', open = open' }
            neighs =
              filter (not . (`S.member` closed'). getClass env) $
              getNeighbours env current
            env'' = foldr (process current) env' neighs
        in algorithm' env''


process :: (Ord node, Ord eqNode, Show node) =>
  node -> node -> Environment node eqNode -> Environment node eqNode
process current neighbor env =
  let tentativeGScore = gScore env current + 1
      open' = S.insert neighbor (open env)
      gscr = gScore env neighbor
  in
    if tentativeGScore >= gscr
    then env { open = open' }
    else env { open = open'
             , cameFrom = M.insert neighbor current (cameFrom env)
             , gScores = M.insert neighbor tentativeGScore (gScores env)
             , fScores =
                 M.insert
                   neighbor
                   (gscr + scoreHeuristic env neighbor)
                   (fScores env)
             }


finished :: Environment node eqNode -> node -> Bool
finished env = isGoal $ params env


getClass :: Environment node eqNode -> node -> eqNode
getClass env = eqClass $ params env


getNeighbours :: Environment node eqNode -> node -> [node]
getNeighbours env = neighbours $ params env


scoreHeuristic :: Environment node eqNode -> node -> Int
scoreHeuristic env = heuristic $ params env


findCurrent :: Ord node => Environment node eqNode -> Maybe node
findCurrent env =
  let set = open env
  in
    if S.null set
    then Nothing
    else Just $ minimumBy (compare `on` fScore env) set


fScore :: Ord node => Environment node eqNode -> node -> Int
fScore env node =
  case M.lookup node (fScores env) of
    Nothing -> maxBound
    Just sc -> sc


gScore :: Ord node => Environment node eqNode -> node -> Int
gScore env node =
  case M.lookup node (gScores env) of
    Nothing -> maxBound
    Just sc -> sc


constructPath :: Ord node => Environment node eqNode -> node -> Path node
constructPath env node =
  case M.lookup node (cameFrom env) of
    Nothing -> [node]
    Just from -> node : constructPath env from
