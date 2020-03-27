module Input
  ( intoLabeledGraph
  , WithHash(..)
  , Input(..)
  , NodeProp(..)
  , EdgeProp(..)
  ) where

import           Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           Protolude

import           Graph           (LabeledGraph, mkLabeledGraph)

data WithHash a =
  WithHash Int a
  deriving (Show, Eq, Ord)

intoLabeledGraph :: Input -> LabeledGraph Int (WithHash Text)
intoLabeledGraph (Input nodes edges) =
  mkLabeledGraph (buildEdges (length nodes) edges) (buildNodes nodes)

buildNodes :: [NodeProp] -> M.Map Int (S.Set (WithHash Text))
buildNodes props = M.fromList $ [1 ..] `zip` (toLabels . nodeLabels <$> props)

toLabels :: [Text] -> S.Set (WithHash Text)
toLabels = S.fromList . fmap transform
  where
    transform element = WithHash (hash element) element

buildEdges :: Int -> [EdgeProp] -> M.Map (Int, Int) (S.Set (WithHash Text))
buildEdges lastNode = M.fromListWith S.union . fmap edgePropToKv . filterOOB
  where
    edgePropToKv (EdgeProp from' to' labels) =
      (toKey from' to', toLabels labels)
    toKey from' to' =
      if from' < to'
        then (from', to')
        else (to', from')
    filterOOB =
      filter
        (\(EdgeProp from' to' _) ->
           from' >= 1 && from' <= lastNode && to' >= 1 && to' <= lastNode)

data Input =
  Input
    { nodeProps :: [NodeProp]
    , edgeProps :: [EdgeProp]
    }
  deriving (Show, Generic)

newtype NodeProp =
  NodeProp
    { nodeLabels :: [Text]
    }
  deriving (Show, Generic)

data EdgeProp =
  EdgeProp
    { from       :: Int
    , to         :: Int
    , edgeLabels :: [Text]
    }
  deriving (Show, Generic)

instance FromJSON EdgeProp

instance FromJSON Input

instance FromJSON NodeProp
