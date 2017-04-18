{-# LANGUAGE CPP #-}
module System.Metrics.Prometheus.Metric.Gauge
       ( Gauge
       , GaugeSample (..)
       , new
       , add
       , sub
       , inc
       , dec
       , set
       , sample
       ) where

import           Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef,
                             newIORef, readIORef)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif


newtype Gauge = Gauge { unGauge :: IORef Double }
newtype GaugeSample = GaugeSample { unGaugeSample :: Double }


new :: IO Gauge
new = Gauge <$> newIORef 0


add :: Double -> Gauge -> IO ()
add x = flip atomicModifyIORef' f . unGauge
  where f v = (v + x, ())


sub :: Double -> Gauge -> IO ()
sub x = flip atomicModifyIORef' f . unGauge
  where f v = (v - x, ())


inc :: Gauge -> IO ()
inc = add 1


dec :: Gauge -> IO ()
dec = sub 1


set :: Double -> Gauge -> IO ()
set x = flip atomicWriteIORef x . unGauge


sample :: Gauge -> IO GaugeSample
sample = fmap GaugeSample . readIORef . unGauge
