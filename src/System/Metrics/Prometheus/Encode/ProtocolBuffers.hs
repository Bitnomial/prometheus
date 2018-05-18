{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module System.Metrics.Prometheus.Encode.ProtocolBuffers
  ( encodeMetrics
  , metricsRequest
  ) where

import           Control.Lens.Operators
import           Data.ByteString.Lazy.Builder               (Builder,
                                                             toLazyByteString)
import qualified Data.Map                                   as Map
import           Data.ProtoLens                             (def)
import           Data.ProtoLens.Encoding                    (buildMessageDelimited)
import           Lens.Labels.Unwrapped                      ()
import           Network.HTTP.Client                        (Request,
                                                             RequestBody (..),
                                                             requestBody,
                                                             requestHeaders)
import           Network.Wreq.Types                         (Putable (..))
import           Proto.Proto.Metrics

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


encodeMetric :: MetricId -> MetricSample -> MetricFamily
encodeMetric (MetricId (Name name) (Labels labels)) = go
  where
    base = def @MetricFamily & #name .~ name
    baseMetric = def & #label .~ labels'
    labels' =
      Map.foldrWithKey
        (\n v -> ((def & #name .~ n & #value .~ v) :))
        []
        labels
    go (CounterMetricSample (Counter.CounterSample i)) =
      base & #type' .~ COUNTER
           & #metric .~
             [ baseMetric & #counter . #value .~
                 fromIntegral i
             ]
    go (GaugeMetricSample (Gauge.GaugeSample i)) =
      base & #type' .~ GAUGE
           & #metric .~
             [ baseMetric & #gauge . #value .~ i
             ]
    go (HistogramMetricSample (Histogram.HistogramSample buckets s count)) =
      base & #type' .~ HISTOGRAM
           & #metric .~
             [ baseMetric & #histogram .~
                 (def & #sampleCount .~ fromIntegral count
                      & #sampleSum .~ s
                      & #bucket .~
                          Map.foldrWithKey
                            (\ub c -> ((def & #cumulativeCount .~
                                                round c
                                            & #upperBound .~ ub
                                       ) :))
                            []
                            buckets
                 )
             ]
    go (SummaryMetricSample (Summary.SummarySample quantiles s count)) =
      base & #type' .~ SUMMARY
           & #metric .~
             [ baseMetric & #summary .~
               (def & #sampleCount .~ fromIntegral count
                    & #sampleSum .~ fromIntegral s
                    & #quantile .~
                        Map.foldrWithKey
                          (\q v -> ((def & #quantile .~ q
                                         & #value .~ fromIntegral v
                                    ) :))
                          []
                          quantiles
               )
             ]
