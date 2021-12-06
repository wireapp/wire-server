{ mkDerivation, aeson, amazonka, amazonka-core, amazonka-s3
, attoparsec, auto-update, base, base64-bytestring, bilge
, bytestring, bytestring-conversion, cargohold-types
, case-insensitive, conduit, conduit-extra, containers, cryptonite
, data-default, errors, exceptions, extended, hpack, HsOpenSSL
, HsOpenSSL-x509-system, http-client, http-client-openssl
, http-client-tls, http-types, imports, lens, lib, metrics-wai
, mime, optparse-applicative, resourcet, retry, safe, swagger
, tagged, tasty, tasty-hunit, text, time, tinylog, types-common
, unordered-containers, uri-bytestring, uuid, wai, wai-conduit
, wai-extra, wai-predicates, wai-routing, wai-utilities, wire-api
, yaml
}:
mkDerivation {
  pname = "cargohold";
  version = "1.5.0";
  src = /home/axeman/workspace/wire-server/services/cargohold;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson amazonka amazonka-core amazonka-s3 attoparsec auto-update
    base base64-bytestring bilge bytestring bytestring-conversion
    cargohold-types case-insensitive conduit conduit-extra containers
    cryptonite data-default errors exceptions extended HsOpenSSL
    HsOpenSSL-x509-system http-client http-client-openssl http-types
    imports lens metrics-wai mime optparse-applicative resourcet retry
    safe swagger text time tinylog types-common unordered-containers
    uri-bytestring uuid wai wai-conduit wai-extra wai-predicates
    wai-routing wai-utilities wire-api yaml
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base base64-bytestring bilge bytestring bytestring-conversion
    cargohold-types containers data-default errors exceptions extended
    HsOpenSSL http-client http-client-tls http-types imports lens
    metrics-wai mime optparse-applicative safe tagged tasty tasty-hunit
    text time types-common uuid wai-utilities yaml
  ];
  prePatch = "hpack";
  description = "Asset Storage API";
  license = lib.licenses.agpl3Only;
}
