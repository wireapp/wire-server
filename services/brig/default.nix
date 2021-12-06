{ mkDerivation, aeson, amazonka, amazonka-dynamodb, amazonka-ses
, amazonka-sqs, async, attoparsec, auto-update, base, base-prelude
, base16-bytestring, base64-bytestring, bilge, bloodhound
, brig-types, bytestring, bytestring-conversion, cargohold-types
, case-insensitive, cassandra-util, comonad, conduit, containers
, cookie, cryptobox-haskell, currency-codes, data-default
, data-timeout, dns, dns-util, either, email-validate
, enclosed-exceptions, errors, exceptions, extended, federator
, filepath, fsnotify, galley-types, geoip2, gundeck-types, hashable
, HaskellNet, HaskellNet-SSL, hpack, hscim, HsOpenSSL
, HsOpenSSL-x509-system, html-entities, http-api-data, http-client
, http-client-openssl, http-client-tls, http-media, http-types
, imports, insert-ordered-containers, interpolate, iproute, iso639
, lens, lens-aeson, lib, metrics-core, metrics-wai, mime, mime-mail
, mmorph, MonadRandom, mtl, multihash, mwc-random, network
, network-conduit-tls, optparse-applicative, pem, polysemy
, polysemy-wire-zoo, proto-lens, QuickCheck, random, random-shuffle
, raw-strings-qq, resource-pool, resourcet, retry, ropes, safe
, safe-exceptions, saml2-web-sso, scientific, scrypt, servant
, servant-client, servant-client-core, servant-server
, servant-swagger, servant-swagger-ui, sodium-crypto-sign, spar
, split, ssl-util, statistics, stomp-queue, string-conversions
, swagger, swagger2, tagged, tasty, tasty-cannon, tasty-hunit
, tasty-quickcheck, template, temporary, text, text-icu-translit
, time, tinylog, transformers, types-common, types-common-aws
, types-common-journal, unliftio, unordered-containers
, uri-bytestring, uuid, vector, wai, wai-extra
, wai-middleware-gunzip, wai-predicates, wai-route, wai-routing
, wai-utilities, warp, warp-tls, wire-api, wire-api-federation
, yaml, zauth
}:
mkDerivation {
  pname = "brig";
  version = "1.35.0";
  src = /home/axeman/workspace/wire-server/services/brig;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson amazonka amazonka-dynamodb amazonka-ses amazonka-sqs async
    attoparsec auto-update base base-prelude base16-bytestring
    base64-bytestring bilge bloodhound brig-types bytestring
    bytestring-conversion cassandra-util comonad conduit containers
    cookie cryptobox-haskell currency-codes data-default data-timeout
    dns dns-util either enclosed-exceptions errors exceptions extended
    filepath fsnotify galley-types geoip2 gundeck-types hashable
    HaskellNet HaskellNet-SSL HsOpenSSL HsOpenSSL-x509-system
    html-entities http-client http-client-openssl http-media http-types
    imports insert-ordered-containers interpolate iproute iso639 lens
    lens-aeson metrics-core metrics-wai mime mime-mail mmorph
    MonadRandom mtl multihash mwc-random network network-conduit-tls
    optparse-applicative pem polysemy polysemy-wire-zoo proto-lens
    random-shuffle resource-pool resourcet retry ropes safe
    safe-exceptions saml2-web-sso scientific scrypt servant
    servant-client servant-server servant-swagger servant-swagger-ui
    sodium-crypto-sign split ssl-util statistics stomp-queue
    string-conversions swagger swagger2 tagged template text
    text-icu-translit time tinylog transformers types-common
    types-common-journal unliftio unordered-containers uri-bytestring
    uuid vector wai wai-extra wai-middleware-gunzip wai-predicates
    wai-routing wai-utilities warp wire-api wire-api-federation yaml
    zauth
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson async attoparsec base base16-bytestring bilge bloodhound
    brig-types bytestring bytestring-conversion cargohold-types
    case-insensitive cassandra-util containers cookie data-timeout
    email-validate exceptions extended federator filepath galley-types
    gundeck-types hscim HsOpenSSL http-api-data http-client
    http-client-tls http-types imports lens lens-aeson metrics-wai mime
    MonadRandom mtl network optparse-applicative pem proto-lens
    QuickCheck random random-shuffle raw-strings-qq retry safe
    saml2-web-sso servant servant-client servant-client-core spar
    string-conversions tasty tasty-cannon tasty-hunit temporary text
    time tinylog transformers types-common types-common-aws
    types-common-journal unliftio unordered-containers uri-bytestring
    uuid vector wai wai-extra wai-route wai-utilities warp warp-tls
    wire-api wire-api-federation yaml zauth
  ];
  testHaskellDepends = [
    aeson base bloodhound brig-types containers dns dns-util http-types
    imports polysemy polysemy-wire-zoo retry servant-client-core tasty
    tasty-hunit tasty-quickcheck time tinylog types-common unliftio
    uri-bytestring uuid wai-utilities wire-api wire-api-federation
  ];
  prePatch = "hpack";
  description = "User Service";
  license = lib.licenses.agpl3Only;
}
