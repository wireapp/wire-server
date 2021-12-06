{ mkDerivation, aeson, aeson-pretty, aeson-qq, attoparsec, base
, base64-bytestring, binary, bytestring, bytestring-conversion
, case-insensitive, cassandra-util, cassava, cereal, comonad
, containers, cookie, cryptonite, currency-codes, deriving-aeson
, deriving-swagger2, directory, email-validate, errors, extended
, extra, filepath, generic-random, generics-sop, ghc-prim, hashable
, hostname-validate, hpack, hscim, http-api-data, http-media
, http-types, imports, insert-ordered-containers, iproute
, iso3166-country-codes, iso639, lens, lib, memory, metrics-wai
, mime, mtl, pem, pretty, proto-lens, protobuf, QuickCheck
, quickcheck-instances, resourcet, saml2-web-sso, schema-profunctor
, servant, servant-client, servant-client-core, servant-multipart
, servant-server, servant-swagger, servant-swagger-ui, singletons
, sop-core, string-conversions, swagger, swagger2, tasty
, tasty-expected-failure, tasty-hunit, tasty-quickcheck, text, time
, types-common, unordered-containers, uri-bytestring, uuid, vector
, wai, wire-message-proto-lens, x509
}:
mkDerivation {
  pname = "wire-api";
  version = "0.1.0";
  src = /home/axeman/workspace/wire-server/libs/wire-api;
  libraryHaskellDepends = [
    aeson attoparsec base base64-bytestring binary bytestring
    bytestring-conversion case-insensitive cassandra-util cassava
    cereal comonad containers cookie cryptonite currency-codes
    deriving-aeson deriving-swagger2 email-validate errors extended
    extra filepath generic-random generics-sop ghc-prim hashable
    hostname-validate hscim http-api-data http-media http-types imports
    insert-ordered-containers iproute iso3166-country-codes iso639 lens
    memory metrics-wai mime mtl pem proto-lens protobuf QuickCheck
    quickcheck-instances resourcet saml2-web-sso schema-profunctor
    servant servant-client servant-client-core servant-multipart
    servant-server servant-swagger servant-swagger-ui singletons
    sop-core string-conversions swagger swagger2 text time types-common
    unordered-containers uri-bytestring uuid vector wai
    wire-message-proto-lens x509
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson aeson-pretty aeson-qq base bytestring bytestring-conversion
    case-insensitive cassava containers currency-codes directory
    filepath hscim imports iso3166-country-codes iso639 lens
    metrics-wai mime pem pretty proto-lens QuickCheck saml2-web-sso
    servant servant-swagger-ui string-conversions swagger2 tasty
    tasty-expected-failure tasty-hunit tasty-quickcheck text time
    types-common unordered-containers uri-bytestring uuid vector
    wire-message-proto-lens
  ];
  prePatch = "hpack";
  license = lib.licenses.agpl3Only;
}
