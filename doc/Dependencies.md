# Dependencies

This page documents how to install necessary dependencies to work with the wire-server code base.

In addition to the information below, you can also consult the Dockerfiles for Alpine Linux, that could serve as inspiration:

* [alpine setup for Haskell services](../build/alpine/Dockerfile.builder)
* [alpine setup for nginz](../services/nginz/Dockerfile)

## General package dependencies (needed to compile Haskell services)

### Fedora:

```bash
sudo dnf install -y pkgconfig haskell-platform libstdc++-devel libstdc++-static gcc-c++ libtool automake openssl-devel libsodium-devel ncurses-compat-libs libicu-devel GeoIP-devel libxml2-devel snappy-devel protobuf-compiler
```

### Debian/Ubuntu:

_Note_: Debian is not recommended due to this issue when running local integration tests: [#327](https://github.com/wireapp/wire-server/issues/327)*. This issue does not occur with ubuntu.

```bash
sudo apt install pkg-config libsodium-dev openssl-dev libtool automake build-essential libicu-dev libsnappy-dev libgeoip-dev protobuf-compiler libxml2-dev zlib1g-dev-y
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

### Debian Testing/Unstable:
```bash
sudo apt install docker.io docker-compose
```

### Generic:

* [Install docker](https://docker.com)
* [Install docker-compose](https://docs.docker.com/compose/install/)
