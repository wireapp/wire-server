# How to build wire-server

As a prerequisiste install the [nix package manager](https://nixos.org/) and [direnv](https://direnv.net/).

All following commands expect that you've entered the nix-provided build-environment by running `direnv allow`.


1. Create a `.envrc.local` file with these contents

    ```
    export COMPILE_NGINX_USING_NIX=1
    export WIRE_BUILD_WITH_CABAL=1
    ```

   and reload the direnv via `direnv reload`

2. Create a `cabal.project.local`. This file is not included in wire-server because it disables optimization.

   ```
   make cabal.project.local
   ```

   This should be re-run whenver a new local cabal package is added to the cabal project.

Then the following Makefile targets can be used to compile and test wire-server locally:

```
# to compile all binaries to ./dist run
make

# to build and install all of galley's executables
make c package=galley

# also run galley's unit tests
make c package=galley test=1
```

## Troubleshooting

### Linker errors while compiling

Linker errors can occur if the nix-provided build environment (see `nix/` directory) changes. Since cabal is not aware of the changed environment the cached build artifacts in `./dist-newstyle` and `~/.cabal/store/` from previous builds may be invalid causing the linker errors.

Haskell Language Server stores its build artifacts in `~/.cache/hie-bios` (equivalent to the `./dist-newstyle` directory) which become invalid for the same reason.

The easiest course of action is to to remove these directories via:

```
make clean
```

# How to run integration tests

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

```
# build and run galley's integration tests
make ci package=galley

# run galley's integration tests that match a pattern
TASTY_PATTERN="/MLS/" make ci package=galley
```
