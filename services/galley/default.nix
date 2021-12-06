{ mkDerivation, aeson, aeson-qq, amazonka, amazonka-sqs, async
, base, base64-bytestring, bilge, binary, brig-types, bytestring
, bytestring-conversion, case-insensitive, cassandra-util, cassava
, cereal, conduit, containers, cookie, cryptonite, currency-codes
, data-default, data-timeout, enclosed-exceptions, errors
, exceptions, extended, extra, federator, galley-types
, gundeck-types, hpack, HsOpenSSL, HsOpenSSL-x509-system, hspec
, http-client, http-client-openssl, http-client-tls, http-media
, http-types, imports, insert-ordered-containers, lens, lens-aeson
, lib, memory, metrics-wai, mtl, optparse-applicative, pem
, polysemy, polysemy-wire-zoo, proto-lens, protobuf, QuickCheck
, quickcheck-instances, random, raw-strings-qq, resourcet, retry
, safe, safe-exceptions, saml2-web-sso, schema-profunctor, servant
, servant-client, servant-client-core, servant-server
, servant-swagger, servant-swagger-ui, sop-core, split, ssl-util
, stm, string-conversions, swagger, swagger2, tagged, tasty
, tasty-cannon, tasty-hspec, tasty-hunit, tasty-quickcheck, text
, time, tinylog, tls, transformers, types-common
, types-common-journal, unliftio, unordered-containers
, uri-bytestring, uuid, vector, wai, wai-extra
, wai-middleware-gunzip, wai-predicates, wai-routing, wai-utilities
, warp, warp-tls, wire-api, wire-api-federation
, wire-message-proto-lens, yaml
}:
mkDerivation {
  pname = "galley";
  version = "0.83.0";
  src = /home/axeman/workspace/wire-server/services/galley;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson amazonka amazonka-sqs async base base64-bytestring bilge
    binary brig-types bytestring bytestring-conversion case-insensitive
    cassandra-util cassava cereal containers cryptonite currency-codes
    data-default enclosed-exceptions errors exceptions extended extra
    galley-types gundeck-types HsOpenSSL HsOpenSSL-x509-system
    http-client http-client-openssl http-client-tls http-media
    http-types imports insert-ordered-containers lens memory
    metrics-wai mtl optparse-applicative pem polysemy polysemy-wire-zoo
    proto-lens protobuf QuickCheck raw-strings-qq resourcet retry safe
    safe-exceptions saml2-web-sso servant servant-client
    servant-client-core servant-server servant-swagger
    servant-swagger-ui sop-core split ssl-util stm string-conversions
    swagger swagger2 tagged text time tinylog tls transformers
    types-common types-common-journal unliftio unordered-containers
    uri-bytestring uuid vector wai wai-extra wai-middleware-gunzip
    wai-predicates wai-routing wai-utilities warp wire-api
    wire-api-federation
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson aeson-qq amazonka amazonka-sqs async base base64-bytestring
    bilge brig-types bytestring bytestring-conversion case-insensitive
    cassandra-util cassava cereal conduit containers cookie
    currency-codes data-timeout errors exceptions extended extra
    federator galley-types gundeck-types HsOpenSSL
    HsOpenSSL-x509-system hspec http-client http-client-openssl
    http-client-tls http-types imports lens lens-aeson metrics-wai mtl
    optparse-applicative pem proto-lens protobuf QuickCheck
    quickcheck-instances random raw-strings-qq retry safe saml2-web-sso
    schema-profunctor servant servant-client servant-client-core
    servant-server servant-swagger sop-core ssl-util string-conversions
    tagged tasty tasty-cannon tasty-hunit text time tinylog tls
    transformers types-common types-common-journal unliftio
    unordered-containers uri-bytestring uuid vector wai wai-extra
    wai-utilities warp warp-tls wire-api wire-api-federation
    wire-message-proto-lens yaml
  ];
  testHaskellDepends = [
    base case-insensitive containers extended extra galley-types
    http-types imports lens QuickCheck raw-strings-qq safe
    saml2-web-sso servant-client servant-swagger ssl-util tagged tasty
    tasty-hspec tasty-hunit tasty-quickcheck transformers types-common
    wai wai-predicates wire-api wire-api-federation
  ];
  prePatch = "hpack";
  description = "Conversations";
  license = lib.licenses.agpl3Only;
}
