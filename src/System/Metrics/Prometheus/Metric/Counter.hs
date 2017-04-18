{-# LANGUAGE CPP #-}
module System.Metrics.Prometheus.Metric.Counter
       ( Counter
       , CounterSample (..)
       , new
       , add
       , inc
       , sample
       ) where


import           Data.Atomics.Counter (AtomicCounter, incrCounter_, newCounter,
                                       readCounter)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif


newtype Counter = Counter { unCounter :: AtomicCounter }
newtype CounterSample = CounterSample { unCounterSample :: Int }


new :: IO Counter
new = Counter <$> newCounter 0


add :: Int -> Counter -> IO ()
add by | by >= 0 = incrCounter_ by . unCounter
       | otherwise = error "must be >= 0"


inc :: Counter -> IO ()
inc = add 1


sample :: Counter -> IO CounterSample
sample = fmap CounterSample . readCounter . unCounter
