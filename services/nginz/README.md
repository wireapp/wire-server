# NGINX build with extra modules

## Compile natively

To build nginz natively, ensure to have the usual C compiler toolchains installed, along with the following dependencies:

* openssl
* libossp-uuid
* [libzauth](../../libs/libzauth) 
    * depends on the rust compiler, libsodium, [makedeb](../../tools/makedeb)

If you're on alpine, see the [Dockerfile](Dockerfile) for the precise dependency names. If you're on another platform, their names might differ slightly.

Once you have all necessary dependencies, `make` in this directory should work.

## Compile with docker

`make docker`
