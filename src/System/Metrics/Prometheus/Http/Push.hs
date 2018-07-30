{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.Metrics.Prometheus.Http.Push
       ( pushHttpTextMetrics
       )
       where

import           Control.Concurrent                    (threadDelay)
import           Control.Monad                         (forever)
import           Data.ByteString.Builder               (toLazyByteString)
import           Data.Map                              (foldMapWithKey)
import           Data.Text                             (Text, unpack)
import           Network.HTTP.Client                   (Request,
                                                        RequestBody (..),
                                                        requestBody,
                                                        requestHeaders)
import           Network.HTTP.Types                    (hContentType)
import           Network.Wreq.Session                  (newSession, put)
import           Network.Wreq.Types                    (Putable (..))

import           System.Metrics.Prometheus.Encode.Text (encodeMetrics)
import           System.Metrics.Prometheus.MetricId    (Labels (..))
import           System.Metrics.Prometheus.Registry    (RegistrySample)


-- | Push text metrics to a pushgateway.
pushHttpTextMetrics :: String            -- ^ The base URL of the pushgateway, including the port number.
                    -> Text              -- ^ The name of this job.
                    -> Labels            -- ^ The label set to use as a grouping key for these metrics.
                    -> Int               -- ^ Push frequency, in microseconds.
                    -> IO RegistrySample -- ^ The action to get the latest metrics.
                    -> IO ()
pushHttpTextMetrics base job (Labels ls) frequency getSample = do
    session <- newSession
    forever $ getSample >>= put session url >> threadDelay frequency
  where
    url = base ++ "/metrics/job/" ++ unpack job ++
        foldMapWithKey (\k v -> "/" ++ unpack k ++ "/" ++ unpack v) ls


instance Putable RegistrySample where
    putPayload = (pure .) . metricsRequest


metricsRequest :: RegistrySample -> Request -> Request
metricsRequest s req = req
    { requestBody    = RequestBodyLBS . toLazyByteString $ encodeMetrics s
    , requestHeaders = contentType : requestHeaders req
    }
  where contentType = (hContentType, "text/plain; version=0.0.4")
