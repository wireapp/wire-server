(crypto-libs)=

# Cryptographic Libraries and Sources of Randomness

The wire-server system has a number of different cryptography building
blocks, and uses a number of different sources for randomness.  This
is a list of these sources and how they are used; it may or may NOT be
complete.

- nginx/nginz uses [openssl](https://openssl.org/) for HTTPS server logic.
- [HsOpenSSL](https://hackage.haskell.org/package/HsOpenSSL) is a wrapper around [openssl](https://openssl.org/), which is uses for HTTPS client logic.
- [scrypt](https://hackage.haskell.org/package/scrypt) for password hashing
- polysemy-wire-zoo has an effect for randomness (`Wire.Sem.Random`)
- scim access tokens
- rust libraries for proteus, mls, e2eid

The following libraries that can be found on https://hackage.haskell.org/.  (It would probably be nice to drop some of these dependencies and replace them by others.)

- cryptobox-haskell
- cryptohash-md5
- cryptohash-sha1
- cryptonite
- HaskellNet-SSL
- HsOpenSSL
- http-client-openssl
- http-client-tls
- network-conduit-tls
- Network.Wire.Bot.Crypto
- Network.Wire.Bot.Crypto.Glue
- scrypt
- sodium-crypto-sign
- ssl-util
- tls
- warp-tls
