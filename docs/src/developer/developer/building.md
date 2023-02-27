# How to build wire-server

As a prerequisiste install the [nix package manager](https://nixos.org/) and [direnv](https://direnv.net/). Follow [these instructions](https://wire-server.cachix.org) to setup the Nix cache which will save you many hours of building.

All following commands expect that you've entered the nix-provided build-environment by running `direnv allow`.


1. Create a `cabal.project.local`. This file is not included in wire-server because it disables optimization.


   make cabal.project.local


   This should be re-run whenver a new local cabal package is added to the cabal project.

Then the following Makefile targets can be used to compile and test wire-server locally:


    # to compile all binaries to ./dist run
    make

    # to build and install all of galley's executables
    make c package=galley

    # also run galley's unit tests
    make c package=galley test=1


## Troubleshooting

### If the PR doesn't pass the CI (read check marks on github)

```
make sanitize-pr
```

### Linker errors while compiling

Linker errors can occur if the nix-provided build environment (see `nix/` directory) changes. Since cabal is not aware of the changed environment the cached build artifacts in `./dist-newstyle` and `~/.cabal/store/` from previous builds may be invalid causing the linker errors.

Haskell Language Server stores its build artifacts in `~/.cache/hie-bios` (equivalent to the `./dist-newstyle` directory) which become invalid for the same reason.

The easiest course of action is to to remove these directories via:

```
make full-clean
```

### Cabal can't read index (Did you call checkForUpdates?)

Sometimes abording cabal mid-update can corrupt its index. Deleting `~/.cabal/packages/hackage.haskell.org` will usually do the trick.

As a side-note: `make c` doesn't run `cabal update`, but `make` does, so keep that in mind.


## How to run integration tests

Integration tests require all of the haskell services (brig, galley, cannon, gundeck, proxy, cargohold, spar) to be correctly configured and running, before being able to execute e.g. the `brig-integration` binary. The test for brig also starts nginz, so make sure it has been built before.
These services require most of the deployment dependencies as seen in the architecture diagram to also be available:

- Required internal dependencies:
    - cassandra (with the correct schema)
    - elasticsearch (with the correct schema)
    - redis
- Required external dependencies are the following configured AWS services (or "fake" replacements providing the same API):
    - SES
    - SQS
    - SNS
    - S3
    - DynamoDB
- Required additional software:
    - netcat (in order to allow the services being tested to talk to the dependencies above)

Setting up these real, but in-memory internal and "fake" external dependencies is done easiest using [`docker-compose`](https://docs.docker.com/compose/install/). Run the following in a separate terminal (it will block that terminal, C-c to shut all these docker images down again):

```
deploy/dockerephemeral/run.sh
```

After all containers are up you can use these Makefile targets to run the tests locally:

1. Build and run all integration tests
   ```bash
   make ci
   ```

2. Build and run integration tests for a service (say galley)
   ```bash
   make ci package=galley
   ```

3. Run integration tests written using `tasty` for a service (say galley) that match a pattern
   ```bash
   TASTY_PATTERN="/MLS/" make ci package=galley
   ```
   For more details on pattern formats, see tasty docs: https://github.com/UnkindPartition/tasty#patterns

4. Run integration tests written using `hspec` for a service (say spar) that match a pattern
   ```bash
   HSPEC_MATCH='Scim' make ci package=spar
   ```
   For more details on match formats, see hspec docs: https://hspec.github.io/match.html

5. Run integration tests without any parallelism
   ```bash
   TASTY_NUM_THREADS=1 make ci package=brig
   ```

   `TASTY_NUM_THREADS` can also be set to other values, it defaults to number of cores available.
