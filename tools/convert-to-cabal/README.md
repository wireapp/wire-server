# How to convert the project to cabal

1. Run

   ```bash
   ./tools/convert-to-cabal/generate.sh
   ```

   This will generate these files
   - `cabal.project.freeze`
   - `cabal.project`
   
2. Create a `cabal.project.local` file with

   ``` 
   optimization: False
   ```

   This configures that local builds fast without optimization.

   To make sure Haskell Language Server also builds all projects without optimization run this:

   ```bash
   ./hack/bin/cabal-project-local-template.sh "ghc-options: -O0" >> ./cabal.project.local
   ```

   Note: cabal v2-repl (which is run by hie-bios (HLS)) seem to be ignoring "optimization" flag for local dependencies, this is why we need to specify `ghc-options` explicitely.


# How to use the project with cabal

1. Update your environment.
   ```bash
   cabal update
   ```

   Add this to your .envrc.local
   ```bash
   export WIRE_BUILD_WITH_CABAL=1
   ```

   You should be able to build wire-server with cabal now:

   ```bash
   make install # using cabal
   make c package=brig # to build and install all of brig's executables
   make c package=brig test=1 # also run unit tests
   make ci package=brig pattern="delete" # build and run brig's integration tests
   ```

2. For Haskell Language Server change `hie.yaml` to use cabal
   ```bash
   WIRE_BUILD_WITH_CABAL=1 make hie.yaml
   ```



## Notes

- `cabal v2-repl` (used by hie-bios) seem to be ignoring "optimization" flag for local dependencies. However it respects ghc-options

```
package foo
  ghc-options: -O0
```

- With new cabal build there doesn't seem to be any way of running tests as part of a build. You have to run the tests manually.
      https://github.com/haskell/cabal/issues/7267

- Nix integration (`nix: True` in `~/.cabal/config`) is not supported in new-build.
  https://github.com/haskell/cabal/issues/4646
  That's why you have to enter the environment defined by `direnv.nix` manually (or via direnv) to use cabal.

- cabal oddity? Specifying `--ghc-options` twice yields different result

    if run
    ```
    cabal build --ghc-options "-O0"  exe:brig
    ```

    and then
    ```
    cabal build --ghc-options "-O0" --ghc-options "-O0"  exe:brig
    ```
    Cabal will retry to build brig and _all_ of its dependencies
