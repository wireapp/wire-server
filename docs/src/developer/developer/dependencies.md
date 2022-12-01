# Dependencies

Reference: {#DevDeps}

This page documents how to install necessary dependencies to work with the wire-server code base.

## General package dependencies (needed to compile Haskell services)

*Note: all the below sections for getting compile-time dependencies necessary to compile all of wire-server may potentially go out of date; if you spot a mistake please open an issue or PR*

### Nix + Direnv

1. Install [Nix](https://nixos.org/download.html). macOS users with a recent
   Mac and non-NixOS Linux users should follow [these
   instructions](https://nixos.org/manual/nix/stable/installation/installing-binary.html)
   to install a binary distribution of Nix.

   Debian users (running at least Debian 11) can use this distro's `nix-bin`
   package. In this case, ensure your user is added to the `nix-users` group
   (and restart your login session to pick this up). Note that Debian's Nix
   package provides a *multi-user* installation (as opposed to a single-user
   installation, which is an option when using the binary distribution tarball).
2. Add the `wire-server` Cachix cache to your system. On NixOS, this can be
   done by adding the following snippet to your system configuration:
   ```nix
   {
     nix.binaryCachePublicKeys = [ "wire-server.cachix.org-1:fVWmRcvdsqzKek3X5Ad8nYNsBSjKZ9Um2NMLfMLS77Y=" ];
     nix.binaryCaches = [ "https://wire-server.cachix.org" ];
   }
   ```
   On non-NixOS systems, these need to be manually added to your `nix.conf`
   (this is at `~/.config/nix/nix.conf` for single-user installations, and
   `/etc/nix/nix.conf` for multi-user installations; you can create this file if
   it doesn't exist):
   ```
   substituters = https://cache.nixos.org https://wire-server.cachix.org
   trusted-public-keys = wire-server.cachix.org-1:fVWmRcvdsqzKek3X5Ad8nYNsBSjKZ9Um2NMLfMLS77Y= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
   ```
3. Install [Direnv](https://direnv.net/).
   On Debian, you can install the `direnv` package. On MacOS use `brew install direnv`.
   * Make sure direnv is hooked into your shell via its appripriate `rc` file.
     Add `eval "$(direnv hook bash|zsh|fish)"` to your ~/.(bash|zsh|fish)rc .

   On NixOS with home-manager, you can set `programs.direnv.enable = true;`,
   which does all of the above.

   See the [Installation documentation](https://direnv.net/docs/installation.html)
   for further details.

   When successfully installed and hooked, direnv should ask you to `direnv allow`
   the current `.envrc` when you cd to this repository.
4. As this repo uses git submodules, ensure you're fetching from them automatically.
   Run `git config --global submodule.recurse true` to enable this globally.
   Run `git config --global fetch.parallel 10` to fetch more submodules in parallel.
   If you've already cloned this repo before having enabled this, run `git
   submodule update --init --recursive` once.

#### Stack's Nix Integration
Using Stack's
[Nix integration](https://docs.haskellstack.org/en/stable/nix_integration/),
Stack will take care of installing any system dependencies automatically -
including `cryptobox-c`. If new system dependencies are needed, add them to the
`stack-deps.nix` file in the project root.

### Fedora:

```bash
sudo dnf install -y pkgconfig haskell-platform libstdc++-devel libstdc++-static gcc-c++ libtool automake openssl-devel libsodium-devel ncurses-compat-libs libicu-devel GeoIP-devel libxml2-devel snappy-devel protobuf-compiler
```

### Ubuntu / Debian:

_Note_: Debian is not recommended due to this issue when running local integration tests: [#327](https://github.com/wireapp/wire-server/issues/327). This issue does not occur with Ubuntu.

```bash
sudo apt install pkg-config libsodium-dev openssl-dev libtool automake build-essential libicu-dev libsnappy-dev libgeoip-dev protobuf-compiler libxml2-dev zlib1g-dev libtinfo-dev liblzma-dev libpcre3 libpcre3-dev -y
```

If `openssl-dev` does not work for you, try `libssl-dev`.

### Arch:

    ```
    # You might also need 'sudo pacman -S base-devel' if you haven't
    # installed the base-devel group already.
    sudo pacman -S geoip snappy icu openssl ncurses-compat-libs
    ```

### macOS:

```bash
brew install pkg-config libsodium openssl automake icu4c geoip snappy protobuf
```

_Note_: macOS users will need to make sure to link Haskell services against a more recent version of OpenSSL than what ships with the OS by default. Additionally, `icu4c` is installed in a non-standard location by `homebrew`. Add the following to your `.stack/config.yaml`:

```yaml
extra-include-dirs:
- /usr/local/opt/openssl/include
- /usr/local/opt/icu4c/include

extra-lib-dirs:
- /usr/local/opt/openssl/lib
- /usr/local/opt/icu4c/lib
```

_Note_: if you're getting `fatal error: 'libxml/parser.h' file not found` and you're on macOS Mojave, try doing:

```bash
sudo installer -pkg /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg -target /
```

## Haskell Stack

Please refer to [Stack's installation instructions](https://docs.haskellstack.org/en/stable/README/#how-to-install).

When you're done, ensure `stack --version` is the same as `STACK_VERSION` in [`build/ubuntu/Dockerfile.prebuilder`](https://github.com/wireapp/wire-server/blob/develop/build/ubuntu/Dockerfile.prebuilder).

If you have to, you can downgrade stack with this command:

```bash
stack upgrade --binary-version <version>
```

### Ubuntu / Debian
_Note_: The packaged versions of `haskell-stack` are too old. It is recommended to follow the generic instructions or to use stack to update stack (`stack upgrade`).

```bash
sudo apt install haskell-stack -y
```

### Generic

   ```bash
   curl -sSL https://get.haskellstack.org/ | sh
   # or
   wget -qO- https://get.haskellstack.org/ | sh
   ```

## Rust

### Ubuntu / Debian
```bash
sudo apt install rustc cargo -y
```

### Generic

```bash
curl https://sh.rustup.rs -sSf | sh
source $HOME/.cargo/env
```

## Formatting Haskell files

You need [`ormolu`](https://github.com/tweag/ormolu) on your PATH, get it with `stack install ormolu`

## Generating license headers

We use [`headroom`](https://github.com/vaclavsvejcar/headroom), get it with `stack install headroom`

## cryptobox-c

### Ubuntu / Debian

```bash
git clone https://github.com/wireapp/cryptobox-c && cd cryptobox-c
make dist
dpkg -i target/release/cryptobox*.deb
```

### Generic
    ```bash
    export TARGET_LIB="$HOME/.wire-dev/lib"
    export TARGET_INCLUDE="$HOME/.wire-dev/include"
    mkdir -p "$TARGET_LIB"
    mkdir -p "$TARGET_INCLUDE"
    git clone https://github.com/wireapp/cryptobox-c && cd cryptobox-c
    make install
    
    # Add cryptobox-c to ldconfig
    sudo bash -c "echo \"${TARGET_LIB}\" > /etc/ld.so.conf.d/cryptobox.conf"
    sudo ldconfig
    ```

Make sure stack knows where to find it. In `~/.stack/config.yaml` add:

(using `~` or `$HOME` doesn't work, needs full paths)

```yaml
extra-include-dirs:
- /usr/local/include
- <YOUR_HOME_DIR>/.wire-dev/include

extra-lib-dirs:
- /usr/local/lib
- <YOUR_HOME_DIR>/.wire-dev/lib
```

## Docker

_Note_: While it is possible to use non-docker solutions to set up and configure this software, we recommend using docker and our provided docker images to configure dependent services rapidly, and ensure a consistent environment for all potential developers.

### Ubuntu / Debian Testing/Unstable:
```bash
sudo apt install docker.io docker-compose
```

After installing docker-io, add your user to the docker group, and restart your shell (usually involving a restart of your graphical environment).

once you've logged in again, if you would like to upload any docker images (optional):
```bash
docker login --username=<MY_DOCKER_USERNAME>
```

### Generic:

* [Install docker](https://docker.com)
* [Install docker-compose](https://docs.docker.com/compose/install/)

## Telepresence

You can instead use [telepresence](https://www.telepresence.io) to allow you to talk to services installed in a given kubernetes namespace on a local or remote kubernetes cluster using easy DNS names like: `curl http://elasticsearch:9200`.

Requirements:

* install telepresence (e.g. `nix-env -iA nixpkgs.telepresence`)
* you need access to a kubernetes cluster
* you need a namespace in which you have installed something (e.g. `make kube-integration-setup` will do this)

### Telepresence example usage:

    ```
    # terminal 1
    telepresence --namespace "$NAMESPACE" --also-proxy cassandra-ephemeral
    ```

    ```
    # terminal 2
    curl http://elasticsearch-ephemeral:9200
    ```

### Telepresence example usage 2:

    ```
    # just one terminal
    telepresence --namespace "$NAMESPACE" --also-proxy cassandra-ephemeral --run bash -c "curl http://elasticsearch-ephemeral:9200"
    ```

### Telepresence usage discussion:

* If you have `fake-aws` and `databases-ephemeral` helm charts set up, you can run either `brig` and other services locally (they connect to cassandra-inside-kubernetes)
* If you also have `brig` and other haskell services running in kubernetes (e.g. you ran `make kube-integration-setup`, you can use telepresence to only run test executables (like `brig-integration`) locally which connect to services inside kubernetes.

In both cases, you need to adjust the various integration configuration files and names so that this can work.

## Helm chart development, integration tests in kubernetes

You need `kubectl`, `helm`, `helmfile`, and a valid kubernetes context. Refer to https://docs.wire.com for details.
