{ mkDerivation, aeson, attoparsec, base, bytestring
, bytestring-conversion, cassandra-util, containers
, deriving-swagger2, imports, lib, QuickCheck, servant-server
, servant-swagger, string-conversions, swagger2, tasty
, tasty-quickcheck, text, time, tinylog, types-common
, unordered-containers, wire-api
}:
mkDerivation {
  pname = "brig-types";
  version = "1.35.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring bytestring-conversion
    cassandra-util containers deriving-swagger2 imports QuickCheck
    servant-server servant-swagger string-conversions swagger2 text
    time tinylog types-common unordered-containers wire-api
  ];
  testHaskellDepends = [
    aeson attoparsec base bytestring bytestring-conversion containers
    imports QuickCheck swagger2 tasty tasty-quickcheck text time
    tinylog types-common unordered-containers wire-api
  ];
  description = "User Service";
  license = lib.licenses.agpl3Only;
}
