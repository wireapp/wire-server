# libzauth

## Compile and install natively

To build libzauth natively, you will need to have the usual C compiler toolchains installed, as well as a Rust toolchain (i.e. through [rustup](https://rustup.rs/)).

### Ubuntu / Debian

#### Building / Installing

```bash
make dist
sudo dpkg -i libzauth*.deb
sudo ldconfig
```

### Generic

#### Building / Installing

```bash
make install
```
