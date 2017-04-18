{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

module System.Metrics.Prometheus.RegistryT where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), Applicative)
import Control.Monad.Trans.State.Strict (runStateT)
#endif
import           Control.Monad.IO.Class                     (MonadIO, liftIO)
import           Control.Monad.Trans.Class                  (MonadTrans)
import           Control.Monad.Trans.State.Strict           (StateT (..),
                                                             evalStateT,
                                                             execStateT, get)

import           System.Metrics.Prometheus.Metric.Counter   (Counter)
import           System.Metrics.Prometheus.Metric.Gauge     (Gauge)
import           System.Metrics.Prometheus.Metric.Histogram (Histogram)
import qualified System.Metrics.Prometheus.Metric.Histogram as Histogram
import           System.Metrics.Prometheus.MetricId         (Labels, Name)
import           System.Metrics.Prometheus.Registry         (Registry,
                                                             RegistrySample,
                                                             new)
import qualified System.Metrics.Prometheus.Registry         as R


newtype RegistryT m a =
    RegistryT { unRegistryT :: StateT Registry m a }
    deriving (Monad, MonadTrans, Applicative, Functor, MonadIO)


evalRegistryT :: Monad m => RegistryT m a -> m a
evalRegistryT = flip evalStateT new . unRegistryT


execRegistryT :: Monad m => RegistryT m a -> m Registry
execRegistryT = flip execStateT new . unRegistryT


runRegistryT :: RegistryT m a -> m (a, Registry)
runRegistryT = flip runStateT new . unRegistryT


withRegistry :: (Registry -> m (a, Registry)) -> RegistryT m a
withRegistry = RegistryT . StateT


registerCounter :: MonadIO m => Name -> Labels -> RegistryT m Counter
registerCounter n l = withRegistry (liftIO . R.registerCounter n l)


registerGauge :: MonadIO m => Name -> Labels -> RegistryT m Gauge
registerGauge n l = withRegistry (liftIO . R.registerGauge n l)


registerHistogram :: MonadIO m => Name -> Labels -> [Histogram.UpperBound] -> RegistryT m Histogram
registerHistogram n l u = withRegistry (liftIO . R.registerHistogram n l u)


#if __GLASGOW_HASKELL__ < 710
sample :: (Monad m, Functor m) => RegistryT m (IO RegistrySample)
#else
sample :: Monad m => RegistryT m (IO RegistrySample)
#endif
sample = R.sample <$> RegistryT get
