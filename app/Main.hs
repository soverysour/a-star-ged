module Main
  ( main
  ) where

import           Data.Aeson
import           Data.ByteString.Lazy (readFile, writeFile)
import           Data.Text            (pack)
import           Protolude            hiding (readFile, writeFile)
import           Protolude.Panic      (panic)

import           Algo
import           Graph
import           Input
import           Output

readGraph :: FilePath -> IO (LabeledGraph Int (WithHash Text))
readGraph path = do
  contentBs <- readFile path
  case decode contentBs of
    Nothing    -> panic $ "Could not decode " <> pack path <> " contents."
    Just input -> pure $ intoLabeledGraph input

writeResult :: FilePath -> GedCost -> IO ()
writeResult path cost = writeFile path (encode cost)

main :: IO ()
main = do
  graphLhs <- readGraph "./input-graph-before.json"
  graphRhs <- readGraph "./input-graph-after.json"
  let gedCost = calculateGed graphLhs graphRhs
  writeResult "./output-ged.json" gedCost
