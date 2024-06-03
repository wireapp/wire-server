# Profiling Wire

This documents one possible avenue to profile Wire Server, be it as a whole or individual services such as Galley or Brig.

For that we are using `cabal.project.local` to define that we want to build with profiling on. added at the top of the file:

```haskell
profiling: True
benchmarks: True
```

We can then recompile.

```sh
make c
```

If we are only profiling a single service, Galley for our example, we can run galley for profiling as such, where `xx` is the profiling flag we want:

```sh
cd services/galley
../../dist/galley +RTS -xx -l -RTS -c galley.integration.yaml`
```

*for more info on flags, see [here](https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html#rts-options-for-heap-profiling).*

Once galley is running and its port is bound, running tests as usual will use this galley.

```sh
make ci-safe package=galley
```

This will generate an eventlog file. We can use `eventlog2html` to visualise it better.

```
cabal install eventlog2html
eventlog2html galley.eventlog
```

Open the result `html` file and enjoy.
