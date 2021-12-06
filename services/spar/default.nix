{ mkDerivation, aeson, aeson-qq, attoparsec, base
, base64-bytestring, bilge, binary, brig-types, bytestring
, bytestring-conversion, case-insensitive, cassandra-util, cassava
, conduit, containers, cookie, cryptonite, data-default
, email-validate, exceptions, extended, galley-types, ghc-prim
, hpack, hscim, HsOpenSSL, hspec, hspec-discover, hspec-wai
, http-api-data, http-client, http-media, http-types, imports
, insert-ordered-containers, lens, lens-aeson, lib, memory
, metrics-wai, MonadRandom, mtl, network-uri, optparse-applicative
, polysemy, polysemy-check, polysemy-plugin, QuickCheck, random
, raw-strings-qq, retry, saml2-web-sso, servant, servant-multipart
, servant-server, servant-swagger, silently, string-conversions
, swagger2, tasty-hunit, text, text-latin1, time, tinylog
, transformers, types-common, unordered-containers, uri-bytestring
, uuid, vector, wai, wai-extra, wai-utilities, warp, wire-api, x509
, xml-conduit, yaml, zauth
}:
mkDerivation {
  pname = "spar";
  version = "0.1";
  src = /home/axeman/workspace/wire-server/services/spar;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-qq attoparsec base base64-bytestring bilge binary
    brig-types bytestring bytestring-conversion case-insensitive
    cassandra-util containers cookie cryptonite data-default
    email-validate exceptions extended galley-types ghc-prim hscim
    HsOpenSSL http-api-data http-client http-media http-types imports
    insert-ordered-containers lens memory metrics-wai mtl network-uri
    optparse-applicative polysemy polysemy-plugin raw-strings-qq retry
    saml2-web-sso servant servant-multipart servant-server
    servant-swagger string-conversions swagger2 text text-latin1 time
    tinylog transformers types-common unordered-containers
    uri-bytestring uuid wai wai-utilities warp wire-api x509
    xml-conduit yaml
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson aeson-qq attoparsec base base64-bytestring bilge binary
    brig-types bytestring bytestring-conversion case-insensitive
    cassandra-util cassava conduit containers cookie cryptonite
    data-default email-validate exceptions extended galley-types
    ghc-prim hscim HsOpenSSL hspec hspec-wai http-api-data http-client
    http-media http-types imports insert-ordered-containers lens
    lens-aeson memory metrics-wai MonadRandom mtl network-uri
    optparse-applicative polysemy polysemy-plugin QuickCheck random
    raw-strings-qq retry saml2-web-sso servant servant-multipart
    servant-server servant-swagger silently string-conversions swagger2
    tasty-hunit text text-latin1 time tinylog transformers types-common
    unordered-containers uri-bytestring uuid vector wai wai-extra
    wai-utilities warp wire-api x509 xml-conduit yaml zauth
  ];
  executableToolDepends = [ hspec-discover ];
  testHaskellDepends = [
    aeson aeson-qq attoparsec base base64-bytestring bilge binary
    brig-types bytestring bytestring-conversion case-insensitive
    cassandra-util containers cookie cryptonite data-default
    email-validate exceptions extended galley-types ghc-prim hscim
    HsOpenSSL hspec http-api-data http-client http-media http-types
    imports insert-ordered-containers lens lens-aeson memory
    metrics-wai mtl network-uri optparse-applicative polysemy
    polysemy-check polysemy-plugin QuickCheck raw-strings-qq retry
    saml2-web-sso servant servant-multipart servant-server
    servant-swagger string-conversions swagger2 text text-latin1 time
    tinylog transformers types-common unordered-containers
    uri-bytestring uuid wai wai-utilities warp wire-api x509
    xml-conduit yaml
  ];
  testToolDepends = [ hspec-discover ];
  prePatch = "hpack";
  description = "User Service for SSO (Single Sign-On) provisioning and authentication";
  license = lib.licenses.agpl3Only;
}
