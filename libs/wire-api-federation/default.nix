{ mkDerivation, aeson, aeson-pretty, async, base, bytestring
, bytestring-conversion, case-insensitive, containers, either
, errors, exceptions, hpack, hspec, hspec-discover, http-types
, http2, HUnit, imports, lib, lifted-base, metrics-wai, mtl
, network, QuickCheck, retry, servant, servant-client
, servant-client-core, servant-server, sop-core, streaming-commons
, template-haskell, text, time, time-manager, tls, types-common
, uuid, wai-utilities, wire-api
}:
mkDerivation {
  pname = "wire-api-federation";
  version = "0.1.0";
  src = /home/axeman/workspace/wire-server/libs/wire-api-federation;
  libraryHaskellDepends = [
    aeson async base bytestring bytestring-conversion case-insensitive
    containers either errors exceptions http-types http2 imports
    lifted-base metrics-wai mtl network QuickCheck servant
    servant-client servant-client-core servant-server sop-core
    streaming-commons template-haskell text time time-manager tls
    types-common wai-utilities wire-api
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson aeson-pretty async base bytestring bytestring-conversion
    case-insensitive containers either errors exceptions hspec
    http-types http2 HUnit imports lifted-base metrics-wai mtl network
    QuickCheck retry servant servant-client servant-client-core
    servant-server sop-core streaming-commons template-haskell text
    time time-manager tls types-common uuid wai-utilities wire-api
  ];
  testToolDepends = [ hspec-discover ];
  prePatch = "hpack";
  description = "The Wire server-to-server API for federation";
  license = lib.licenses.agpl3Only;
}
