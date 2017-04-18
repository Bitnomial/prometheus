{-# LANGUAGE OverloadedStrings #-}

module System.Metrics.Prometheus.Encode
       ( encodeMetrics
       , serializeMetrics
       ) where

import           Data.ByteString.Builder                    (Builder,
                                                             toLazyByteString)
import           Data.ByteString.Lazy                       (ByteString)
import           Data.Function                              (on)
import           Data.List                                  (groupBy,
                                                             intersperse)
import qualified Data.Map                                   as Map
import           Data.Monoid                                ((<>), mconcat)

import           System.Metrics.Prometheus.Encode.Histogram (encodeHistogram)
import           System.Metrics.Prometheus.Encode.MetricId  (encodeDouble,
                                                             encodeHeader,
                                                             encodeInt,
                                                             encodeMetricId,
                                                             newline, space)
import           System.Metrics.Prometheus.Metric           (MetricSample (..),
                                                             metricSample)
import           System.Metrics.Prometheus.Metric.Counter   (CounterSample (..))
import           System.Metrics.Prometheus.Metric.Gauge     (GaugeSample (..))
import           System.Metrics.Prometheus.MetricId         (MetricId (..))
import           System.Metrics.Prometheus.Registry         (RegistrySample (..))


serializeMetrics :: RegistrySample -> ByteString
serializeMetrics = toLazyByteString . encodeMetrics


encodeMetrics :: RegistrySample -> Builder
encodeMetrics = (<> newline) . mconcat . intersperse newline . map encodeMetricGroup
    . groupByName . Map.toList . unRegistrySample
  where groupByName = groupBy ((==) `on` (name . fst))


encodeMetricGroup :: [(MetricId, MetricSample)] -> Builder
encodeMetricGroup group = encodeHeader mid sample <> newline
    <> mconcat (intersperse newline $ map encodeMetric group)
  where
    (mid, sample) = head group


encodeMetric :: (MetricId, MetricSample) -> Builder
encodeMetric (mid, sample) = metricSample (encodeCounter mid) (encodeGauge mid)
    (encodeHistogram mid) (encodeSummary mid) sample
  where
    encodeSummary = undefined


encodeCounter :: MetricId -> CounterSample -> Builder
encodeCounter mid counter = encodeMetricId mid <> space <> encodeInt (unCounterSample counter)


encodeGauge :: MetricId -> GaugeSample -> Builder
encodeGauge mid gauge = encodeMetricId mid <> space <> encodeDouble (unGaugeSample gauge)
