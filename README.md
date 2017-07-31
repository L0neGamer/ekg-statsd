# ekg-statsd: statsd backend for ekg [![Hackage version](https://img.shields.io/hackage/v/ekg-statsd.svg?label=Hackage)](https://hackage.haskell.org/package/ekg-statsd) [![Build Status](https://secure.travis-ci.org/tibbe/ekg-statsd.svg?branch=master)](http://travis-ci.org/tibbe/ekg-statsd)

This library lets you send metrics gathered by the ekg family of
packages (e.g. ekg-core and ekg) to
[statsd](https://github.com/etsy/statsd/). While statsd fulfills a
very similar role to ekg, it supports many more backends/graphing
systems (e.g. Graphite). By sending your metrics to statsd, you can
have your ekg metrics appear in these systems.

# Getting started

Exporting metrics to statsd is simple. Either create an empty metric
store and register some metrics

    import System.Metrics
    import System.Remote.Monitoring.Statsd

    main = do
        store <- newStore
        registerGcMetrics store
        forkStatsd defaultStatsdOptions store
        ...

or use the default metrics and metric store provided by the ekg
package

    import System.Remote.Monitoring
    import System.Remote.Monitoring.Statsd

    main = do
        handle <- forkServer "localhost" 8000
        forkStatsd defaultStatsdOptions (serverMetricStore handle)
        ...

`forkStatsd` starts a new thread the will periodically send your
metrics to statsd using UDP.

# Get involved!

Please report bugs via the
[GitHub issue tracker](https://github.com/tibbe/ekg-statsd/issues).

Master [git repository](https://github.com/tibbe/ekg-statsd):

    git clone https://github.com/tibbe/ekg-statsd.git

# Authors

This library is written and maintained by Johan Tibell,
<johan.tibell@gmail.com>.
