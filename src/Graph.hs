module Graph
  ( LabeledGraph
  , ExistenceOr(..)
  , mkLabeledGraph
  , getEdge
  , getNodeInfo
  , getNodes
  , getNeighbours
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           Protolude

data ExistenceOr l
  = Itself l
  | Existence
  deriving (Show, Eq, Ord)

-- Invariant - (n, n) pairs are ordered and elements are present accordingly, in graph member and nodes member.
mkLabeledGraph ::
     (Ord n, Ord l)
  => M.Map (n, n) (S.Set l)
  -> M.Map n (S.Set l)
  -> LabeledGraph n l
mkLabeledGraph graph nodes =
  let addExistence = S.insert Existence . S.map Itself
      rememberNeighbours acc (n1, n2) =
        remembering n2 n1 (remembering n1 n2 acc)
        where
          remembering a b = M.insertWith S.union a (S.singleton b)
      graph' = M.map addExistence graph
      nodes' = M.map addExistence nodes
      neighbours = foldl' rememberNeighbours M.empty (M.keys graph')
   in LabeledGraph graph' nodes' neighbours

data LabeledGraph n l =
  LabeledGraph
    { unGraph      :: M.Map (n, n) (S.Set (ExistenceOr l))
    , unNodes      :: M.Map n (S.Set (ExistenceOr l))
    , unNeighbours :: M.Map n (S.Set n)
    }
  deriving (Show)

getEdge :: Ord n => (n, n) -> LabeledGraph n l -> S.Set (ExistenceOr l)
getEdge (lhs, rhs) =
  if lhs < rhs
    then getEdge' (lhs, rhs)
    else getEdge' (rhs, lhs)
  where
    getEdge' node graph = M.findWithDefault S.empty node (unGraph graph)

getNodeInfo :: Ord n => LabeledGraph n l -> n -> S.Set (ExistenceOr l)
getNodeInfo graph node = M.findWithDefault S.empty node (unNodes graph)

getNodes :: LabeledGraph n l -> [n]
getNodes = M.keys . unNodes

getNeighbours :: Ord n => LabeledGraph n l -> n -> S.Set n
getNeighbours graph node = M.findWithDefault S.empty node (unNeighbours graph)
