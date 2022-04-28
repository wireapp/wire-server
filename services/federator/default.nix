{ mkDerivation, aeson, async, base, bilge, binary, bytestring
, bytestring-conversion, connection, constraints, containers
, cryptonite, data-default, directory, dns, dns-util, either
, errors, exceptions, extended, filepath, hinotify, hspec
, http-client, http-client-openssl, http-client-tls, http-media
, http-types, http2, imports, interpolate, kan-extensions, lens
, lib, metrics-core, metrics-wai, mtl, network, network-uri
, optparse-applicative, pem, polysemy, polysemy-wire-zoo
, QuickCheck, random, retry, servant, servant-client
, servant-client-core, streaming-commons, string-conversions, tasty
, tasty-hunit, tasty-quickcheck, temporary, text, time-manager
, tinylog, tls, transformers, types-common, unix, uri-bytestring
, uuid, wai, wai-extra, wai-utilities, warp, warp-tls, wire-api
, wire-api-federation, x509, x509-store, x509-system
, x509-validation, yaml
}:
mkDerivation {
  pname = "federator";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bilge binary bytestring bytestring-conversion
    constraints containers data-default dns dns-util either exceptions
    extended filepath hinotify http-client http-client-openssl
    http-media http-types http2 imports kan-extensions lens
    metrics-core metrics-wai mtl network network-uri pem polysemy
    polysemy-wire-zoo retry servant servant-client-core
    streaming-commons string-conversions text time-manager tinylog tls
    types-common unix uri-bytestring uuid wai wai-utilities warp
    warp-tls wire-api wire-api-federation x509 x509-store x509-system
    x509-validation
  ];
  executableHaskellDepends = [
    aeson async base bilge binary bytestring bytestring-conversion
    connection constraints containers cryptonite data-default dns
    dns-util either errors exceptions extended filepath hinotify hspec
    http-client http-client-openssl http-client-tls http-media
    http-types http2 imports kan-extensions lens metrics-core
    metrics-wai mtl network network-uri optparse-applicative pem
    polysemy polysemy-wire-zoo QuickCheck random retry servant
    servant-client-core streaming-commons string-conversions tasty
    tasty-hunit text time-manager tinylog tls types-common unix
    uri-bytestring uuid wai wai-utilities warp warp-tls wire-api
    wire-api-federation x509 x509-store x509-system x509-validation
    yaml
  ];
  testHaskellDepends = [
    aeson async base bilge binary bytestring bytestring-conversion
    constraints containers data-default directory dns dns-util either
    exceptions extended filepath hinotify http-client
    http-client-openssl http-media http-types http2 imports interpolate
    kan-extensions lens metrics-core metrics-wai mtl network
    network-uri pem polysemy polysemy-wire-zoo QuickCheck retry servant
    servant-client servant-client-core streaming-commons
    string-conversions tasty tasty-hunit tasty-quickcheck temporary
    text time-manager tinylog tls transformers types-common unix
    uri-bytestring uuid wai wai-extra wai-utilities warp warp-tls
    wire-api wire-api-federation x509 x509-store x509-system
    x509-validation yaml
  ];
  description = "Federation Service";
  license = lib.licenses.agpl3Only;
}
