module System.Metrics.Prometheus.Metric.Summary where

import Data.Map.Strict (Map)


data SummarySample = SummarySample
    { sumQuantiles :: !(Map Double Int)
    , sumSum :: !Int
    , sumCount :: !Int
    }
    deriving Show
