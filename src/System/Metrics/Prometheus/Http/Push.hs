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
import           Network.HTTP.Client                   (Request (..),
                                                        RequestBody (..),
                                                        defaultManagerSettings,
                                                        httpNoBody, newManager,
                                                        requestBody,
                                                        requestFromURI,
                                                        requestHeaders)
import           Network.HTTP.Types                    (hContentType, methodPut)
import           Network.URI                           (URI (..), URIAuth,
                                                        nullURI)

import           System.Metrics.Prometheus.Encode.Text (encodeMetrics)
import           System.Metrics.Prometheus.MetricId    (Labels (..))
import           System.Metrics.Prometheus.Registry    (RegistrySample)


-- | Push text metrics to a pushgateway.
pushHttpTextMetrics :: URIAuth           -- ^ PushGateway URI name, including port number (ex: myGateway.com:8080)
                    -> Text              -- ^ Job name
                    -> Labels            -- ^ Label set to use as a grouping key for metrics
                    -> Int               -- ^ Microsecond push frequency
                    -> IO RegistrySample -- ^ Action to get latest metrics
                    -> IO ()
pushHttpTextMetrics gatewayName jobName labels frequencyMicros getSample = do
    manager    <- newManager defaultManagerSettings
    requestUri <- requestFromURI $ buildUri gatewayName jobName labels
    forever $ getSample >>= flip httpNoBody manager . request requestUri >> threadDelay frequencyMicros
  where
    request req sample = req
        { method         = methodPut
        , requestBody    = RequestBodyLBS . toLazyByteString $ encodeMetrics sample
        , requestHeaders = [(hContentType, "text/plain; version=0.0.4")]
        }

buildUri :: URIAuth -> Text -> Labels -> URI
buildUri gatewayName jobName (Labels ls) = nullURI
    { uriScheme    = "http:"
    , uriAuthority = Just gatewayName
    , uriPath      = "/metrics/job/" ++ unpack jobName ++ foldMapWithKey labelPath ls
    }
  where labelPath k v = "/" ++ unpack k ++ "/" ++ unpack v
