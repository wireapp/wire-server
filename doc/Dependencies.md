# Dependencies

This page documents how to install necessary dependencies to work with the wire-server code base.

In addition to the information below, you can also consult the Dockerfiles for Alpine Linux, that could serve as inspiration:

* [alpine setup for Haskell services](../build/alpine/Dockerfile.builder)
* [alpine setup for nginz](../services/nginz/Dockerfile)

### General Package dependencies (needed to compile Haskell services)

#### Fedora:

```bash
sudo dnf install -y pkgconfig haskell-platform libstdc++-devel libstdc++-static gcc-c++ libtool automake openssl-devel libsodium-devel ncurses-compat-libs libicu-devel GeoIP-devel libxml2-devel snappy-devel protobuf-compiler
```

#### Debian:

*Note: Debian is not recommended due to this issue when running local integration tests: [#327](https://github.com/wireapp/wire-server/issues/327)*

```bash
sudo apt install pkg-config libsodium-dev openssl-dev libtool automake build-essential libicu-dev libsnappy-dev libgeoip-dev protobuf-compiler libxml2-dev -y
```

If `openssl-dev` does not work for you, try `libssl-dev`.

#### Ubuntu:

Hopefully almost like Debian.

#### Arch:

```
# You might also need 'sudo pacman -S base-devel' if you haven't
# installed the base-devel group already.
sudo pacman -S geoip snappy icu openssl
```

#### macOS:

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

### Haskell Stack

```bash
curl -sSL https://get.haskellstack.org/ | sh
# or
wget -qO- https://get.haskellstack.org/ | sh
```

Ensure `stack --version` is >= 1.6.5

You may wish to make executables installed by stack available, by e.g. adding the following to your shell profile:

```bash
export PATH=~/.local/bin:$PATH
```

### Rust

```bash
curl https://sh.rustup.rs -sSf | sh
source $HOME/.cargo/env
```

### cryptobox-c

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

### makedeb

Create debian packages, optional, only used in `make dist`

```bash
git clone https://github.com/wireapp/wire-server && cd wire-server/tools/makedeb
stack install
```

### Docker

* [Install docker](https://docker.com)
* [Install docker-compose](https://docs.docker.com/compose/install/)
