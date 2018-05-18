# Prometheus Haskell Client

A simple and modern, type safe, idiomatic Haskell client for
[Prometheus](http://prometheus.io) monitoring. Specifically there is
no use of unsafe IO or manual ByteString construction from lists of
bytes. Batteries-included web server.  .  [Usage Example]

- [Hackage Package](https://hackage.haskell.org/package/prometheus)
- [Github](http://github.com/LukeHoersten/prometheus)

## Building
First you'll have to get [protoc](https://github.com/google/protobuf). Visit
the linked repo and download a binary for your platform and put it in a place
in your `PATH`. Next get the `proto-lens-protoc` package. You can use stack
for this.

```bash
stack install proto-lens-protoc
```

Then run the generation script.

```bash
./generate-code
```

Then build with stack.

```bash
stack build
```

## Usage Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Example where

import           Control.Monad.IO.Class                         (liftIO)
import           System.Metrics.Prometheus.Concurrent.Http      (serveHttpTextMetricsT)
import           System.Metrics.Prometheus.Concurrent.RegistryT
import           System.Metrics.Prometheus.Metric.Counter       (inc)
import           System.Metrics.Prometheus.MetricId

main :: IO ()
main = runRegistryT $ do
    -- Labels can be defined as lists or added to an empty label set
    connectSuccessGauge <- registerGauge "example_connections" (fromList [("login", "success")])
    connectFailureGauge <- registerGauge "example_connections" (addLabel "login" "failure" mempty)
    connectCounter <- registerCounter "example_connection_total" mempty
    latencyHistogram <- registerHistogram "example_round_trip_latency_ms" mempty [10, 20..100]

    liftIO $ inc connectCounter -- increment a counter

    -- [...] pass metric handles to the rest of the app

    serveHttpTextMetricsT 8080 ["metrics"] -- http://localhost:8080/metric server
```

## Advanced Usage

A `Registry` and `StateT`-based `RegistryT` are available for unit
testing or generating lists of `[IO a]` actions that can be
`sequenced` and returned from pure code to be applied.

## Tasks

- [ ] Implement help docstrings.
- [ ] Implement GHC-specific metrics.
- [ ] Implement [summary metric](https://github.com/prometheus/client_golang/blob/master/prometheus/summary.go).
- [ ] Encode name and labels on register.
- [x] Implement ReaderT for Concurrent Registry.
- [x] Library documentation and example.
- [ ] [Name and label validation](http://prometheus.io/docs/concepts/data_model/#metric-names-and-labels)
