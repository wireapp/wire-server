# NGINX build with extra modules

## Compile natively

To build nginz natively, you will need to have the usual C compiler toolchains installed, along with the following dependencies:

* gpg (needed to verify nginx's signatures)
* openssl
* libossp-uuid
* libpcre3
* [libzauth](../../libs/libzauth)
    * depends on the rust compiler, libsodium23

### Alpine
If you're on alpine, see the [Dockerfile](Dockerfile) for the precise dependency names.

### Ubuntu / Debian (backports / testing / unstable)

_Note_: Debian packages are only used as part of wire's infrastructure, and as such, you do not need to install them to run the integration tests or the demo.

_Note_: Debian stable does not contain a new enough version of libsodium. you must get it from backports, testing, or unstable.

_Note_: On some Ubuntu versions, upstart is installed in addition to systemd, causing runit to fail with an error like "Unable to connect to Upstart: Failed to connect to socket". Luckily, there is [a simple fix](https://forum.peppermintos.com/index.php?topic=5210.0).

#### Build Dependencies:
```bash
sudo apt install libossp-uuid-dev libpcre3-dev libsodium23 runit gnupg
```

#### Building
```bash
make
```

### Compile with docker

`make docker`

### Generic
If you're on another platform, the names of the dependencies might differ slightly.

Once you have all necessary dependencies, `make` in this directory should work.

## Common problems while compiling

```
gpg: Can't check signature: public key not found
```

This means that you haven't imported the public key that was used to sign nginx. Look for the keys at https://nginx.org/en/pgp_keys.html and make sure to import ALL of them with:

`gpg --import <paths_to_keys>`

Alternatively, you can ask GPG to find the key by its ID (printed in the error message):

`gpg --recv-keys KEY_ID`

---

```
checking for OpenSSL library ... not found
[...]
./configure: error: SSL modules require the OpenSSL library.
You can either do not enable the modules, or install the OpenSSL library
into the system, or build the OpenSSL library statically from the source
with nginx by using --with-openssl=<path> option.
```

openssl is required to compile nginx and it may be installed in a "non-standard" path in your system. Once you are sure you have installed it, look for `EXTRA_CC_INC` and `EXTRA_CC_LIB` in the `Makefile` and point them to the correct location in your system.

If you are using macOS and you used `brew` to install openssl, the `Makefile` already contains the right paths so you should not be seeing that error.

## How to run it

Have a look at our demo config in [services demo](../../deploy/services-demo/conf/nginz)
