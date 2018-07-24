{-# LANGUAGE OverloadedStrings #-}

module System.Metrics.Prometheus.Encode.ProtocolBuffers
  ( encodeMetrics
  , metricsRequest
  ) where

import           Control.Lens.Operators
import           Data.ByteString.Lazy.Builder (Builder, toLazyByteString)
import qualified Data.Map                     as Map
import           Data.ProtoLens               (def)
import           Data.ProtoLens.Encoding      (buildMessageDelimited)
import           Network.HTTP.Client          (Request, RequestBody (..),
                                               requestBody, requestHeaders)
import           Network.Wreq.Types           (Putable (..))
import qualified Proto.Proto.Metrics          as Proto
import qualified Proto.Proto.Metrics_Fields   as Proto

import           System.Metrics.Prometheus.Metric           (MetricSample (..))
import qualified System.Metrics.Prometheus.Metric.Counter   as Counter
import qualified System.Metrics.Prometheus.Metric.Gauge     as Gauge
import qualified System.Metrics.Prometheus.Metric.Histogram as Histogram
import qualified System.Metrics.Prometheus.Metric.Summary   as Summary
import           System.Metrics.Prometheus.MetricId         (Labels (..),
                                                             MetricId (MetricId),
                                                             Name (..))
import           System.Metrics.Prometheus.Registry         (RegistrySample (..))


instance Putable RegistrySample where
    putPayload = (pure .) . metricsRequest


metricsRequest :: RegistrySample -> Request -> Request
metricsRequest s req = req
    { requestBody    = RequestBodyLBS . toLazyByteString $ encodeMetrics s
    , requestHeaders = contentType : requestHeaders req
    }
  where contentType =
            ( "Content-Type"
            , "application/vnd.google.protobuf; proto=io.prometheus.client.MetricFamily; encoding=delimited"
            )


encodeMetrics :: RegistrySample -> Builder
encodeMetrics = Map.foldMapWithKey ((buildMessageDelimited .) . encodeMetric) . unRegistrySample


encodeMetric :: MetricId -> MetricSample -> Proto.MetricFamily
encodeMetric (MetricId (Name name) (Labels labels)) = go
  where
    base :: Proto.MetricFamily
    base = def & Proto.name .~ name
    baseMetric = def & Proto.label .~ labels'
    labels' =
      Map.foldrWithKey
        (\n v -> ((def & Proto.name .~ n & Proto.value .~ v) :))
        []
        labels
    go (CounterMetricSample (Counter.CounterSample i)) =
      base & Proto.type' .~ Proto.COUNTER
           & Proto.metric .~
             [ baseMetric & Proto.counter . Proto.value .~
                 fromIntegral i
             ]
    go (GaugeMetricSample (Gauge.GaugeSample i)) =
      base & Proto.type' .~ Proto.GAUGE
           & Proto.metric .~
             [ baseMetric & Proto.gauge . Proto.value .~ i
             ]
    go (HistogramMetricSample (Histogram.HistogramSample buckets s count)) =
      base & Proto.type' .~ Proto.HISTOGRAM
           & Proto.metric .~
             [ baseMetric & Proto.histogram .~
                 (def & Proto.sampleCount .~ fromIntegral count
                      & Proto.sampleSum .~ s
                      & Proto.bucket .~
                          Map.foldrWithKey
                            (\ub c -> ((def & Proto.cumulativeCount .~
                                                round c
                                            & Proto.upperBound .~ ub
                                       ) :))
                            []
                            buckets
                 )
             ]
    go (SummaryMetricSample (Summary.SummarySample quantiles s count)) =
      base & Proto.type' .~ Proto.SUMMARY
           & Proto.metric .~
             [ baseMetric & Proto.summary .~
               (def & Proto.sampleCount .~ fromIntegral count
                    & Proto.sampleSum .~ fromIntegral s
                    & Proto.quantile .~
                        Map.foldrWithKey
                          (\q v -> ((def & Proto.quantile .~ q
                                         & Proto.value .~ fromIntegral v
                                    ) :))
                          []
                          quantiles
               )
             ]
