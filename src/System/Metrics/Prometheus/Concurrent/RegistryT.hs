{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

module System.Metrics.Prometheus.Concurrent.RegistryT where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), Applicative)
import           Control.Monad.Trans.Reader (runReaderT)
#endif
import           Control.Monad.IO.Class                        (MonadIO, liftIO)
import           Control.Monad.Trans.Class                     (MonadTrans)
import           Control.Monad.Trans.Reader                    (ReaderT (..),
                                                                ask)

import           System.Metrics.Prometheus.Concurrent.Registry (Registry, new)
import qualified System.Metrics.Prometheus.Concurrent.Registry as R
import           System.Metrics.Prometheus.Metric.Counter      (Counter)
import           System.Metrics.Prometheus.Metric.Gauge        (Gauge)
import           System.Metrics.Prometheus.Metric.Histogram    (Histogram)
import qualified System.Metrics.Prometheus.Metric.Histogram    as Histogram
import           System.Metrics.Prometheus.MetricId            (Labels, Name)
import           System.Metrics.Prometheus.Registry            (RegistrySample)


newtype RegistryT m a =
    RegistryT { unRegistryT :: ReaderT Registry m a }
    deriving (Monad, MonadTrans, Applicative, Functor, MonadIO)


runRegistryT :: MonadIO m => RegistryT m a -> m a
runRegistryT registry = liftIO new >>= runReaderT (unRegistryT registry)


registerCounter :: MonadIO m => Name -> Labels -> RegistryT m Counter
registerCounter n l = RegistryT ask >>= liftIO . R.registerCounter n l


registerGauge :: MonadIO m => Name -> Labels -> RegistryT m Gauge
registerGauge n l = RegistryT ask >>= liftIO . R.registerGauge n l


registerHistogram :: MonadIO m => Name -> Labels -> [Histogram.UpperBound] -> RegistryT m Histogram
registerHistogram n l b = RegistryT ask >>= liftIO . R.registerHistogram n l b


#if __GLASGOW_HASKELL__ < 710
sample :: (Monad m, Functor m) => RegistryT m (IO RegistrySample)
#else
sample :: Monad m => RegistryT m (IO RegistrySample)
#endif
sample = R.sample <$> RegistryT ask
