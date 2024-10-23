# OpenTelemetry Instrumentation

## Current Status

The following components have been instrumented:
- brig
- galley
- gundeck
- cannon

## Known Issues and future work

- Proper HTTP/2 instrumentation is missing for federator & co - this is related to http/2 outobj in the http2 libraray throwing away all structured information
- Some parts of the service, such as background jobs, may need additional instrumentation. It's currently unclear if these are appearing in the tracing data.
- we need to ingest the data into grafana tempo


## Setup instructions for local use

To view the tracing data:

1. Start Jaeger using Docker:
   ```bash
   docker run --rm --name jaeger \
     -e COLLECTOR_ZIPKIN_HOST_PORT=:9411 \
     -p 6831:6831/udp \
     -p 6832:6832/udp \
     -p 5778:5778 \
     -p 16686:16686 \
     -p 4317:4317 \
     -p 4318:4318 \
     -p 14250:14250 \
     -p 14268:14268 \
     -p 14269:14269 \
     -p 9411:9411 \
     jaegertracing/all-in-one:latest
   ```

2. Start your services or run integration tests.
3. Open the Jaeger UI at [http://localhost:16686/](http://localhost:16686/)

## Relevant Resources

We're using the `hs-opentelemetry-*` family of haskell packages available [here](https://github.com/iand675/hs-opentelemetry).

- [hs-opentelemetry-instrumentation-wai](https://hackage.haskell.org/package/hs-opentelemetry-instrumentation-wai-0.1.0.0/docs/src/OpenTelemetry.Instrumentation.Wai.html#local-6989586621679045744)
- [hs-opentelemetry-sdk](https://hackage.haskell.org/package/hs-opentelemetry-sdk-0.0.3.6)
