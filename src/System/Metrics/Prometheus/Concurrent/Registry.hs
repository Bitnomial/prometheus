module System.Metrics.Prometheus.Concurrent.Registry
       ( Registry
       , new
       , registerCounter
       , registerGauge
       , registerHistogram
       , sample
       ) where


import           Control.Applicative
import           Control.Concurrent.MVar                    (MVar,
                                                             modifyMVarMasked,
                                                             newMVar, withMVar)
import           Data.Tuple                                 (swap)

import           System.Metrics.Prometheus.Metric.Counter   (Counter)
import           System.Metrics.Prometheus.Metric.Gauge     (Gauge)
import           System.Metrics.Prometheus.Metric.Histogram (Histogram,
                                                             UpperBound)
import           System.Metrics.Prometheus.MetricId         (Labels, Name)
import qualified System.Metrics.Prometheus.Registry         as R


newtype Registry = Registry { unRegistry :: MVar R.Registry }


new :: IO Registry
new = Registry <$> newMVar R.new


registerCounter :: Name -> Labels -> Registry -> IO Counter
registerCounter name labels = flip modifyMVarMasked register . unRegistry
  where
    register = fmap swap . R.registerCounter name labels


registerGauge :: Name -> Labels -> Registry -> IO Gauge
registerGauge name labels = flip modifyMVarMasked register . unRegistry
  where
    register = fmap swap . R.registerGauge name labels


registerHistogram :: Name -> Labels -> [UpperBound] -> Registry -> IO Histogram
registerHistogram name labels buckets = flip modifyMVarMasked register . unRegistry
  where
    register = fmap swap . R.registerHistogram name labels buckets


sample :: Registry -> IO R.RegistrySample
sample = flip withMVar R.sample . unRegistry
