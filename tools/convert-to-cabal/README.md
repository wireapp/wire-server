# How to convert the project to cabal

Run

```bash
./tools/convert-to-cabal/generate.sh
```

This will generate these files
- `cabal.project.freeze`
- `cabal.project`

   Note: cabal v2-repl (which is run by hie-bios (HLS)) seem to be ignoring "optimization" flag for local dependencies, this is why we need to specify `ghc-options` explicitely.
