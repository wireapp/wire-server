(sources-of-randomness)=

# Sources of Randomness and Cryptographic Algorithms

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

There are a lot of haskell crypto libraries that we are using.  We should probably pick a subset of these we want to depend on and replace the rest.

```
$ git grep -Hni '\(ssl\|tls\|crypt\|sodium\)' '*.cabal' | grep -v '\(exposed-modules\|description\|name\|synopsis\)' | sort
libs/api-bot/api-bot.cabal:20:    Network.Wire.Bot.Crypto
libs/api-bot/api-bot.cabal:21:    Network.Wire.Bot.Crypto.Glue
libs/api-bot/api-bot.cabal:91:    , cryptobox-haskell      >=0.1.1
libs/api-bot/api-bot.cabal:92:    , cryptonite             >=0.17
libs/api-bot/api-bot.cabal:96:    , HaskellNet-SSL         >=0.3
libs/galley-types/galley-types.cabal:78:    , cryptonite
libs/http2-manager/http2-manager.cabal:41:    , HsOpenSSL
libs/http2-manager/http2-manager.cabal:72:    , HsOpenSSL
libs/jwt-tools/jwt-tools.cabal:6:category:      Cryptography
libs/polysemy-wire-zoo/polysemy-wire-zoo.cabal:88:    , HsOpenSSL
libs/sodium-crypto-sign/sodium-crypto-sign.cabal:18:  other-modules:      Paths_sodium_crypto_sign
libs/sodium-crypto-sign/sodium-crypto-sign.cabal:67:  pkgconfig-depends:  libsodium >=0.4.5
libs/sodium-crypto-sign/sodium-crypto-sign.cabal:6:  FFI bindings to some of the libsodium cryptographic signature functions which are based on Ed25519.
libs/sodium-crypto-sign/sodium-crypto-sign.cabal:8:category:      Cryptography
libs/ssl-util/ssl-util.cabal:16:  other-modules:      Paths_ssl_util
libs/ssl-util/ssl-util.cabal:69:    , HsOpenSSL    >=0.11
libs/types-common/types-common.cabal:104:    , cryptohash-md5         >=0.11.7.2
libs/types-common/types-common.cabal:105:    , cryptohash-sha1        >=0.11.7.2
libs/types-common/types-common.cabal:106:    , cryptonite             >=0.26
libs/wai-utilities/wai-utilities.cabal:102:    , warp-tls
libs/wire-api-federation/wire-api-federation.cabal:87:    , HsOpenSSL
libs/wire-api/wire-api.cabal:232:    , cryptonite
libs/wire-api/wire-api.cabal:247:    , HsOpenSSL
libs/wire-api/wire-api.cabal:273:    , scrypt
libs/wire-api/wire-api.cabal:728:    , cryptonite
libs/zauth/zauth.cabal:150:    , sodium-crypto-sign
libs/zauth/zauth.cabal:217:    , sodium-crypto-sign
libs/zauth/zauth.cabal:83:    , sodium-crypto-sign     >=0.1
services/brig/brig.cabal:210:    , cryptobox-haskell          >=0.1.1
services/brig/brig.cabal:231:    , HaskellNet-SSL             >=0.3
services/brig/brig.cabal:232:    , HsOpenSSL                  >=0.10
services/brig/brig.cabal:235:    , http-client-openssl        >=0.2
services/brig/brig.cabal:256:    , network-conduit-tls
services/brig/brig.cabal:276:    , sodium-crypto-sign         >=0.1
services/brig/brig.cabal:278:    , ssl-util
services/brig/brig.cabal:368:    , HsOpenSSL
services/brig/brig.cabal:551:    , HsOpenSSL
services/brig/brig.cabal:554:    , http-client-tls        >=0.2
services/brig/brig.cabal:612:    , warp-tls               >=3.2
services/brig/brig.cabal:795:    , HsOpenSSL          >=0.10
services/cargohold/cargohold.cabal:102:    , cryptonite             >=0.20
services/cargohold/cargohold.cabal:107:    , HsOpenSSL              >=0.11
services/cargohold/cargohold.cabal:109:    , http-client-openssl    >=0.2
services/cargohold/cargohold.cabal:195:    , HsOpenSSL     >=0.11
services/cargohold/cargohold.cabal:272:    , cryptonite
services/cargohold/cargohold.cabal:276:    , http-client-tls        >=0.2
services/federator/federator.cabal:119:    , HsOpenSSL
services/federator/federator.cabal:206:    , HsOpenSSL
services/federator/federator.cabal:278:    , cryptonite
services/federator/federator.cabal:282:    , HsOpenSSL
services/federator/federator.cabal:284:    , http-client-tls
services/federator/federator.cabal:382:    , HsOpenSSL
services/federator/federator.cabal:410:    , warp-tls
services/galley/galley.cabal:211:    , cryptonite
services/galley/galley.cabal:224:    , HsOpenSSL              >=0.11
services/galley/galley.cabal:226:    , http-client-openssl    >=0.2
services/galley/galley.cabal:254:    , ssl-util               >=0.1
services/galley/galley.cabal:261:    , tls                    >=1.3.10
services/galley/galley.cabal:336:    , HsOpenSSL
services/galley/galley.cabal:438:    , cryptonite
services/galley/galley.cabal:451:    , HsOpenSSL
services/galley/galley.cabal:454:    , http-client-openssl
services/galley/galley.cabal:455:    , http-client-tls
services/galley/galley.cabal:483:    , ssl-util
services/galley/galley.cabal:507:    , warp-tls               >=3.2
services/gundeck/gundeck.cabal:123:    , http-client-tls        >=0.2.2
services/gundeck/gundeck.cabal:140:    , tls                    >=1.3.4
services/gundeck/gundeck.cabal:211:    , HsOpenSSL
services/gundeck/gundeck.cabal:291:    , HsOpenSSL
services/gundeck/gundeck.cabal:293:    , http-client-tls
services/gundeck/gundeck.cabal:465:    , HsOpenSSL
services/gundeck/gundeck.cabal:548:    , HsOpenSSL
services/proxy/proxy.cabal:87:    , http-client-tls     >=0.2
services/spar/spar.cabal:142:    , cryptonite
services/spar/spar.cabal:327:    , cryptonite
services/spar/spar.cabal:333:    , HsOpenSSL
tools/api-simulations/api-simulations.cabal:147:    , http-client-tls  >=0.2
tools/api-simulations/api-simulations.cabal:222:    , http-client-tls       >=0.2
tools/stern/stern.cabal:263:    , HsOpenSSL
tools/stern/stern.cabal:265:    , http-client-tls
```
