{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module System.Metrics.Prometheus.Concurrent.Http
       ( Path
       , serveHttpTextMetrics
       , serveHttpTextMetricsT
       , prometheusApp
       )
       where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif
import           Control.Monad.IO.Class                         (MonadIO,
                                                                 liftIO)
import           Data.Text                                      (Text)
import           Network.HTTP.Types                             (hContentType,
                                                                 methodGet,
                                                                 status200,
                                                                 status404)
import           Network.Wai                                    (Application,
                                                                 Request,
                                                                 Response,
                                                                 pathInfo,
                                                                 requestMethod,
                                                                 responseBuilder,
                                                                 responseLBS)
import           Network.Wai.Handler.Warp                       (Port, run)

import           System.Metrics.Prometheus.Concurrent.RegistryT (RegistryT,
                                                                 sample)
import           System.Metrics.Prometheus.Encode               (encodeMetrics)
import           System.Metrics.Prometheus.Registry             (RegistrySample)


type Path = [Text]


serveHttpTextMetrics :: MonadIO m => Port -> Path -> IO RegistrySample -> m ()
serveHttpTextMetrics port path = liftIO . run port . prometheusApp path


#if __GLASGOW_HASKELL__ < 710
serveHttpTextMetricsT :: (MonadIO m, Functor m) => Port -> Path -> RegistryT m ()
#else
serveHttpTextMetricsT :: MonadIO m => Port -> Path -> RegistryT m ()
#endif
serveHttpTextMetricsT port path = liftIO . serveHttpTextMetrics port path =<< sample


prometheusApp :: Path -> IO RegistrySample -> Application
prometheusApp path runSample request respond
    | isPrometheusRequest path request = respond =<< prometheusResponse <$> runSample
    | otherwise = respond response404
  where
    prometheusResponse = responseBuilder status200 headers . encodeMetrics
    headers = [(hContentType, "text/plain; version=0.0.4")]


response404 :: Response
response404 = responseLBS status404 header404 body404
  where
    header404 = [(hContentType, "text/plain")]
    body404 = "404"


isPrometheusRequest :: Path -> Request -> Bool
isPrometheusRequest path request = isGet && matchesPath
  where
    matchesPath = pathInfo request == path
    isGet = requestMethod request == methodGet
