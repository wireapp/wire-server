# How to build wire-server

## Getting up to speed

### nix

0. make sure you have `git` installed. It will be
   assumed by `nix`. Also make sure to run on an amd64
   machine, `wire-server` is not yet compatible with arm64.

1. Install the [nix package manager](https://nixos.org/download.html).
   Please follow the install instruction provided on their website.

2. Add the `wire-server` `cachix` cache to your system.
   This is best done by using the `cachix` executable, which, as soon as you have
   `nix` itself installed can be run with this (a bit unwieldy) command:

   ```bash
   nix run \
     --experimental-features 'nix-command flakes' \
     github:nixos/nixpkgs/nixpkgs-unstable#cachix -- \
     use wire-server
   ```

### direnv

1. Install [`direnv`](https://direnv.net/). See the [installation documentation](https://direnv.net/docs/installation.html)
   for further details.

### checking out the repo

1. clone the git repo, it can be found at [the wireapp/wire-server github](https://github.com/wireapp/wire-server)
2. initialize this repo's submodules with
   ```bash
   git submodule update --init --recursive
   ```

### run direnv

Now it's time to let nix fetch all dependencies. Enter the `wire-server` checkout, run

```bash
direnv allow
```

and go and grab a coffee. ☕

Your system will likely not build much, but it will definitely
spend some time fetching things from different caches.

### initializing the cabal mirrors

There are a few dependencies that are not provided by the nix env, for these, please run
```bash
cabal update
```
now that you're in the devshell.

### building wire-server

#### with cabal

You can build within the devshell by using the `Makefile` targets and
cabal.
The binaries are then dropped into `./dist/<service-name>`

You may build all services in `wire-server` by running

```bash
make c
```

you may build a single package by running

```bash
make c package=brig
```

you may run the tests by first starting background services with

```bash
./deploy/dockerephemeral/run.sh
```

and then executing

```bash
ulimit 10240 # set your resource limit to some high number
make ci-safe # run the ci
```
If the former command fails, make sure you have a working installation of `docker`
or continue to the troubleshooting section right below.

#### with nix

you may build each individual service by running

```bash
nix build -Lv \
  --experimental-features 'nix-command' \
  -f ./nix wireServer.<service>
```

you may build all the libraries that exist locally or are in the closure of `wire-server` by running

```bash
nix build -Lv \
  --experimental-features 'nix-command' \
  -f ./nix wireServer.haskellPackages.<library>
```

you may build all the images that would be deployed by running 

```bash
nix build -Lv \
  --experimental-features 'nix-command' \
  -f ./nix wireServer.allImages
```

> ℹ️  Info
> 
> if you don't want to pass the `--experimental-features` flag to nix, you may as well
> add this to your `nix.conf` which is documented [in the nix manual](https://nixos.org/manual/nix/unstable/command-ref/conf-file.html)

`nix` puts all the build outputs into the nix store but leaves a link in the `result` directory
that will appear in the same directory that you have run the command in. To find out what
artifacts where build, just run

```bash
ls -l result
```

## Troubleshooting

### If the PR doesn't pass the CI (read check marks on github)

```bash
make sanitize-pr
```

### Linker errors while compiling

Linker errors can occur if the nix-provided build environment (see `nix/` directory) changes. Since cabal is not aware of the changed environment the cached build artifacts in `./dist-newstyle` and `~/.cabal/store/` from previous builds may be invalid causing the linker errors.

Haskell Language Server stores its build artifacts in `~/.cache/hie-bios` (equivalent to the `./dist-newstyle` directory) which become invalid for the same reason.

The easiest course of action is to to remove these directories via:

```bash
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

Furthermore, testing federation requires a local DNS server set up with appropriate SRV records.

Setting up these real, but in-memory internal and "fake" external dependencies is done easiest using [`docker-compose`](https://docs.docker.com/compose/install/). Run the following in a separate terminal (it will block that terminal, C-c to shut all these docker images down again):

```bash
deploy/dockerephemeral/run.sh
```

Also make sure your system is able to resolve the fully qualified domain `localhost.` (note the trailing dot). This is surprisingly not trivial, because of limitations in how libc parses `/etc/hosts`. You can check that with, for example, `ping localhost.`. If you get a name resolution error, you need to add `localhost.` explictly to your `/etc/hosts` file.

After all containers are up you can use these Makefile targets to run the tests locally:

0. Set your resource limits to a high enough number: 
   ```bash
   ulimit 10240
   ```

1. Build and run all integration tests
   ```bash
   make ci-safe
   ```

2. Build and run integration tests for a service (say galley)
   ```bash
   make ci package=galley
   ```

3. Run integration tests written using `tasty` for a service (say galley) that match a pattern
   ```bash
   TASTY_PATTERN="/MLS/" make ci-safe package=galley
   ```
   For more details on pattern formats, see tasty docs: https://github.com/UnkindPartition/tasty#patterns

4. Run integration tests written using `hspec` for a service (say spar) that match a pattern
   ```bash
   HSPEC_MATCH='Scim' make ci-safe package=spar
   ```
   For more details on match formats, see hspec docs: https://hspec.github.io/match.html

5. Run integration tests without any parallelism
   ```bash
   TASTY_NUM_THREADS=1 make ci-safe package=brig
   ```

   `TASTY_NUM_THREADS` can also be set to other values, it defaults to number of cores available.

6. This is a test line for multiversioning of docs.
