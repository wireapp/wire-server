# Dependencies {#DevDeps}

This page documents how to install necessary dependencies to work with the wire-server code base.

In addition to the information below, you can also consult the Dockerfiles for Alpine Linux, that could serve as inspiration:

* [alpine setup for Haskell services](../../build/alpine/Dockerfile.builder)
* [alpine setup for nginz](../../services/nginz/Dockerfile)

## General package dependencies (needed to compile Haskell services)

### Fedora:

```bash
sudo dnf install -y pkgconfig haskell-platform libstdc++-devel libstdc++-static gcc-c++ libtool automake openssl-devel libsodium-devel ncurses-compat-libs libicu-devel GeoIP-devel libxml2-devel snappy-devel protobuf-compiler
```

### Ubuntu / Debian:

_Note_: Debian is not recommended due to this issue when running local integration tests: [#327](https://github.com/wireapp/wire-server/issues/327)*. This issue does not occur with Ubuntu.

```bash
sudo apt install pkg-config libsodium-dev openssl-dev libtool automake build-essential libicu-dev libsnappy-dev libgeoip-dev protobuf-compiler libxml2-dev zlib1g-dev -y
```

If `openssl-dev` does not work for you, try `libssl-dev`.

### Arch:

```
# You might also need 'sudo pacman -S base-devel' if you haven't
# installed the base-devel group already.
sudo pacman -S geoip snappy icu openssl
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

When you're done, ensure `stack --version` is >= 1.6.5

You may wish to make executables installed by stack available, by e.g. adding the following to your shell profile:

```bash
export PATH=~/.local/bin:$PATH
```

### Ubuntu / Debian Unstable
_Note_: Debian stretch packages too old of a version of haskell-stack. It is recommended to retrieve the version available from testing, or unstable, or to use stack to update stack.(https://github.com/commercialhaskell/stack/issues/3686)*

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

You need `ormolu` on your PATH, get it with `stack install ormolu`

## Generating license headers

We use `headroom`, get it with `stack install headroom`

## makedeb

This is a tool to create debian-style binary packages. It is optional, and is only used if you want to install debian-style packages on your debian or ubuntu system.

_Note_: If you want to build debian-style packages of cryptobox-c and other wire utilities, execute this step. otherwise, make sure to execute the 'Generic' version of the cryptobox-c step.

```bash
git clone https://github.com/wireapp/wire-server && cd wire-server/tools/makedeb
export VERSION=0
make dist
dpkg -i ../../dist/makedeb*.deb
```

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
````

### Generic:

* [Install docker](https://docker.com)
* [Install docker-compose](https://docs.docker.com/compose/install/)

## Nix

Using Stack's [Nix integration](https://docs.haskellstack.org/en/stable/nix_integration/), Stack will take care of installing any system
dependencies automatically - including `cryptobox-c`. If new system dependencies are needed, add them to the `shell.nix` file in the project root.
Just type `$ nix-shell` and you will automatically have `make`, `docker-compose` and `stack` in `PATH`.
You can then run all the builds, and the native dependencies will be automatically present.

We are currently on a snapshot that uses `ghc863` but Nix only ships `ghc864` as `ghc863` is officially deprecated. The releases should be totally compatible, but we need to convince stack that this is true. Hence, we can add the following to our global stack config in `~/.stack/config.yaml`
```bash
skip-ghc-check: true
```
