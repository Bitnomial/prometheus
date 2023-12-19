
## 2.3.0

*   Change the `observeAndSample` function from the
    `System.Metrics.Prometheus.Metric.Histogram` module to return the value of
    the sample that was just added, instead of the previous sample.
    This change matches similar functions for `Counter`s and `Gauge`s.
    [#51](https://github.com/bitnomial/prometheus/pull/51)
