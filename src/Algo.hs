{-# LANGUAGE TupleSections #-}

module Algo
  ( calculateGed
  ) where

import qualified Data.Bimap           as M
import qualified Data.PQueue.Prio.Min as Q
import qualified Data.Set             as S
import           Protolude            hiding (from, isLeft)

import           Graph
import           Output

type DiffConfig n l = (LabeledGraph n l, LabeledGraph n l)

calculateGed ::
     (Ord n, Ord l) => LabeledGraph n l -> LabeledGraph n l -> GedCost
calculateGed lhs rhs =
  let configQueue =
        Q.singleton
          0
          (SearchConfig M.empty S.empty S.empty (getNodes lhs) (getNodes rhs))
   in startSearch (lhs, rhs) configQueue

-- Perform<OP> only deals with the first 3 members.
-- The last 2 members are managed by startSearch, until control reaches perform<OP>.
data SearchConfig n =
  SearchConfig
    { mappedNodes    :: M.Bimap n n
    , lhsIntoEpsilon :: S.Set n
    , rhsIntoEpsilon :: S.Set n
    , lhsNodesLeft   :: [n]
    , rhsNodesLeft   :: [n]
    }
  deriving (Eq, Show)

startSearch ::
     (Ord n, Ord l)
  => DiffConfig n l
  -> Q.MinPQueue Int (SearchConfig n)
  -> GedCost
startSearch diffConfig configQueue =
  let ((minCost, currentConfig), configQueue') = Q.deleteFindMin configQueue
      adding configQ (cost', config') = Q.insert cost' config' configQ
      withAcc a f (int, b) c =
        let (int', b') = f a b c
         in (int + int', b')
      extracting configQ = startSearch diffConfig . adding configQ
   in case (lhsNodesLeft currentConfig, rhsNodesLeft currentConfig) of
        ([], []) -> GedCost minCost
        (lh, []) ->
          extracting configQueue' $
          foldl'
            (withAcc diffConfig performDelete)
            (minCost, currentConfig {lhsNodesLeft = []})
            lh
        ([], rh) ->
          extracting configQueue' $
          foldl'
            (withAcc diffConfig performInsert)
            (minCost, currentConfig {rhsNodesLeft = []})
            rh
        (lh:lhs, rhs) ->
          startSearch diffConfig $
          foldl' adding configQueue' $
          withAcc
            diffConfig
            performDelete
            (minCost, currentConfig {lhsNodesLeft = lhs})
            lh :
          ((\(cost, result) -> (cost + minCost, result)) <$>
           foldForUpdates
             lh
             diffConfig
             (currentConfig {lhsNodesLeft = lhs})
             rhs
             [])

foldForUpdates ::
     (Ord n, Ord l)
  => n
  -> DiffConfig n l
  -> SearchConfig n
  -> [n]
  -> [n]
  -> [(Int, SearchConfig n)]
foldForUpdates _ _ _ [] _ = []
foldForUpdates from diffConfig searchConfig (x:xs) before =
  performUpdate from diffConfig (searchConfig {rhsNodesLeft = xs ++ before}) x :
  foldForUpdates from diffConfig searchConfig xs (x : before)

getMappedNeighbours ::
     Ord n => Bool -> LabeledGraph n l -> SearchConfig n -> n -> [n]
getMappedNeighbours isLeft graph config node =
  let neighbours = getNeighbours graph node
      (intoEpsilon, memberM) =
        if isLeft
          then (lhsIntoEpsilon, M.member)
          else (rhsIntoEpsilon, M.memberR)
      isMapped t =
        t `S.member` intoEpsilon config || t `memberM` mappedNodes config
   in S.toList $ S.filter isMapped neighbours

sumCosts ::
     (Functor f, Foldable f, Ord n) => n -> f n -> LabeledGraph n l -> Int
sumCosts node neighbours graph =
  sum $ (\neighbour -> S.size $ getEdge (node, neighbour) graph) <$> neighbours

performDelete ::
     Ord n => DiffConfig n l -> SearchConfig n -> n -> (Int, SearchConfig n)
performDelete (before, _) searchConfig node =
  let nodeCost = S.size $ getNodeInfo before node
      validNeighbours = getMappedNeighbours True before searchConfig node
      edgeCost = sumCosts node validNeighbours before
      totalCost = nodeCost + edgeCost
   in ( totalCost
      , searchConfig
          {lhsIntoEpsilon = S.insert node $ lhsIntoEpsilon searchConfig})

performInsert ::
     Ord n => DiffConfig n l -> SearchConfig n -> n -> (Int, SearchConfig n)
performInsert (_, after) searchConfig node =
  let nodeCost = S.size $ getNodeInfo after node
      validNeighbours = getMappedNeighbours False after searchConfig node
      edgeCost = sumCosts node validNeighbours after
      totalCost = nodeCost + edgeCost
   in ( totalCost
      , searchConfig
          {rhsIntoEpsilon = S.insert node $ rhsIntoEpsilon searchConfig})

safeBimapIx :: (Ord a, Ord b) => M.Bimap a b -> a -> Maybe b
safeBimapIx m a =
  if a `M.member` m
    then Just $ m M.! a
    else Nothing

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe _ Nothing = Nothing
filterMaybe f (Just x)
  | f x = Just x
  | otherwise = Nothing

performUpdate ::
     (Ord n, Ord l)
  => n
  -> DiffConfig n l
  -> SearchConfig n
  -> n
  -> (Int, SearchConfig n)
performUpdate lhs (before, after) searchConfig rhs =
  let diffCost a b = S.size $ exclusion a b
      nodeCost = diffCost (getNodeInfo before lhs) (getNodeInfo after rhs)
      lhsNeighbours = getNeighbours before lhs
      rhsNeighbours = getNeighbours after rhs
      (lhsEpsilonNeighbours, restLhs) =
        S.partition (`S.member` lhsIntoEpsilon searchConfig) lhsNeighbours
      (rhsEpsilonNeighbours, restRhs) =
        S.partition (`S.member` rhsIntoEpsilon searchConfig) rhsNeighbours
      intoProperMatch lhs' =
        (lhs', ) <$>
        filterMaybe
          (`S.member` restRhs)
          (safeBimapIx (mappedNodes searchConfig) lhs')
      properMatches = mapMaybe intoProperMatch $ S.toList restLhs
      deletedLhsCost = sumCosts lhs (S.toList lhsEpsilonNeighbours) before
      insertedRhsCost = sumCosts rhs (S.toList rhsEpsilonNeighbours) after
      getProperMatchCost (from', to') =
        diffCost (getEdge (lhs, from') before) (getEdge (rhs, to') after)
      properMatchesCost = sum $ getProperMatchCost <$> properMatches
      totalCost =
        nodeCost + deletedLhsCost + insertedRhsCost + properMatchesCost
   in ( totalCost
      , searchConfig {mappedNodes = M.insert lhs rhs (mappedNodes searchConfig)})

exclusion :: Ord a => S.Set a -> S.Set a -> S.Set a
exclusion a b = S.difference (S.union a b) (S.intersection b a)
