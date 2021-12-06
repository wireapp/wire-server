{ mkDerivation, aeson, attoparsec, base, bytestring-conversion
, cassandra-util, containers, deriving-swagger2, hpack, imports
, lib, QuickCheck, servant-server, servant-swagger
, string-conversions, swagger2, tasty, tasty-quickcheck, text, time
, tinylog, types-common, unordered-containers, wire-api
}:
mkDerivation {
  pname = "brig-types";
  version = "1.35.0";
  src = /home/axeman/workspace/wire-server/libs/brig-types;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring-conversion cassandra-util
    containers deriving-swagger2 imports QuickCheck servant-server
    servant-swagger string-conversions swagger2 text time tinylog
    types-common unordered-containers wire-api
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson attoparsec base bytestring-conversion containers imports
    QuickCheck swagger2 tasty tasty-quickcheck text time tinylog
    types-common unordered-containers wire-api
  ];
  prePatch = "hpack";
  description = "User Service";
  license = lib.licenses.agpl3Only;
}
