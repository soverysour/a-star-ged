module Main
  ( main
  ) where

import           Data.Aeson
import           Data.ByteString.Lazy (readFile, writeFile)
import           Data.Text            (pack)
import           Protolude            hiding (readFile, writeFile)
import           Protolude.Panic      (panic)

import           Algo
import           AlgoPar
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

-- This is super bad, but Haskell makes it an absolute pain to do this without fetching
-- some parser combinator library (I also use a prelude replacement)
toThreadCount :: Text -> IO Int
toThreadCount "1" = pure 1
toThreadCount "2" = pure 2
toThreadCount "4" = pure 4
toThreadCount "8" = pure 8
toThreadCount tc  = panic $ "Bad thread count given: " <> tc <> "."

runArgs :: LabeledGraph Int (WithHash Text) -> LabeledGraph Int (WithHash Text) -> [Text] -> IO GedCost
runArgs lhs rhs [] = pure $ calculateGed lhs rhs
runArgs lhs rhs ["--run-par-with-k", k] = do
  k' <- toThreadCount k
  pure $ calculateGedPar k' lhs rhs
runArgs _ _ _ = panic "Bad arguments."

main :: IO ()
main = do
  graphLhs <- readGraph "./input-graph-before.json"
  graphRhs <- readGraph "./input-graph-after.json"
  args <- getArgs
  gedCost <- runArgs graphLhs graphRhs $ pack <$> args
  writeResult "./output-ged.json" gedCost

