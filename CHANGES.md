## Unreleased

 * Support GHC 9.0 through 9.8.

## 0.2.5.0 (2020-06-15)

 * Bugfix: when reporting counter values to statsd, send only the
   increments ([#23](https://github.com/tibbe/ekg-statsd/pull/23)).

## 0.2.4.1 (2020-05-21)

 * Sanitize metric names by replacing `:` with `_` to adhere to the StatsD
   protocol ([#26](https://github.com/tibbe/ekg-statsd/pull/26)).
 * Add support for GHC 8.10 in base bounds and CI.

## 0.2.4.0 (2018-08-01)

* Don't rethrow `ThreadKilled` exceptions to the thread that invoked
  `forkStatsd`, so that the statsd thread can be safely killed
  ([#20](https://github.com/tibbe/ekg-statsd/pull/20)).

## 0.2.3.0 (2018-04-10)

 * API addition: 'statsdFlush', allows to flush the sample to statsd
   server manually
   ([#18](https://github.com/tibbe/ekg-statsd/pull/18)).

## 0.2.2.0 (2017-09-25)

 * Remove internal `diffSample` optimisation and always report the
   full state of the `Store` instead of only what's changed between
   iterations ([#17](https://github.com/tibbe/ekg-statsd/pull/17)).

## 0.2.1.1 (2017-07-31)

 * Support GHC 8.2.1.
 * Suppress errors when sending to a non-existent receiver
   ([#6](https://github.com/tibbe/ekg-statsd/pull/6)).

## 0.2.1.0 (2016-08-11)

 * Send distributions as gauges and counters.
 * Update examples.

## 0.2.0.4 (2016-05-28)

 * GHC 8.0 support.

## 0.2.0.3 (2015-06-06)

 * Support GHC 7.10.

## 0.2.0.2 (2015-04-09)

 * Add support for network-2.6.

## 0.2.0.1 (2014-09-30)

 * Add support for text-1.2.

## 0.2.0.0 (2014-05-27)

 * Add configurable metric name prefix and suffix.
 * Add support for GHC 7.4.

## 0.1.0.0 (2014-05-01)

 * Initial release.
