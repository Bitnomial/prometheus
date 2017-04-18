{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

module System.Metrics.Prometheus.MetricId where

import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.String (IsString)
import           Data.Text   (Text)
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (Monoid)
#endif
import           Prelude     hiding (null)


newtype Name = Name { unName :: Text } deriving (Show, Eq, Ord, IsString, Monoid)
newtype Labels = Labels { unLabels :: Map Text Text } deriving (Show, Eq, Ord, Monoid)


data MetricId =
    MetricId
    { name   :: Name
    , labels :: Labels
    } deriving (Eq, Ord, Show)


addLabel :: Text -> Text -> Labels -> Labels
addLabel key val = Labels . Map.insert key val . unLabels


fromList :: [(Text, Text)] -> Labels
fromList = Labels . Map.fromList


toList :: Labels -> [(Text, Text)]
toList = Map.toList . unLabels


null :: Labels -> Bool
null = Map.null . unLabels
