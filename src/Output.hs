module Output
  ( GedCost(..)
  ) where

import           Data.Aeson
import           Protolude

newtype GedCost =
  GedCost
    { getCost :: Int
    }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON GedCost
