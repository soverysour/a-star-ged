module Main
  ( main
  ) where

import           Test.HUnit
import           Protolude
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

import           Algo
import           AlgoPar
import           Graph
import           Output

main :: IO ()
main = void $ runTestTT testCases

testCases :: Test
testCases =
  TestList
    [ TestLabel "Empty graphs yield no cost." testEmptyBoth
    , TestLabel "Something on the RHS only." testEmptyLhs
    , TestLabel "Something on the LHS only." testEmptyRhs
    , TestLabel "Same populated graph." testSameGraph
    , TestLabel "Parallel: Empty graphs yield no cost." testEmptyBoth'
    , TestLabel "Parallel: Something on the RHS only." testEmptyLhs'
    , TestLabel "Parallel: Something on the LHS only." testEmptyRhs'
    , TestLabel "Parallel: Same populated graph." testSameGraph'
    ]

testEmptyBoth :: Test
testEmptyBoth = TestCase $ assertEqual "Empty graphs" (GedCost 0) (calculateGed emptyGraph emptyGraph)

testEmptyLhs :: Test
testEmptyLhs = TestCase $ assertEqual "Empty Lhs" (GedCost 6) (calculateGed emptyGraph someGraph)

testEmptyRhs :: Test
testEmptyRhs = TestCase $ assertEqual "Empty Rhs" (GedCost 6) (calculateGed someGraph emptyGraph)

testSameGraph :: Test
testSameGraph = TestCase $ assertEqual "Same graph" (GedCost 0) (calculateGed someGraph someGraph)

--------------------------------------------------------------------------------

emptyGraph :: LabeledGraph Int Int
emptyGraph = mkLabeledGraph M.empty M.empty

defaultPar :: Int
defaultPar = 5

someGraph :: LabeledGraph Int Int
someGraph = mkLabeledGraph edgeInfo nodeInfo
  where
    nodeInfo = M.fromList [(1, S.empty), (2, S.empty), (3, S.empty)]
    edgeInfo = M.fromList [((1, 2), S.empty), ((2, 3), S.empty), ((1, 3), S.empty)]

--------------------------------------------------------------------------------

testEmptyBoth' :: Test
testEmptyBoth' = TestCase $ assertEqual "Empty graphs" (GedCost 0) (calculateGedPar defaultPar emptyGraph emptyGraph)

testEmptyLhs' :: Test
testEmptyLhs' = TestCase $ assertEqual "Empty Lhs" (GedCost 6) (calculateGedPar defaultPar emptyGraph someGraph)

testEmptyRhs' :: Test
testEmptyRhs' = TestCase $ assertEqual "Empty Rhs" (GedCost 6) (calculateGedPar defaultPar someGraph emptyGraph)

testSameGraph' :: Test
testSameGraph' = TestCase $ assertEqual "Same graph" (GedCost 0) (calculateGedPar defaultPar someGraph someGraph)
