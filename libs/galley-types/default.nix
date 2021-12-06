{ mkDerivation, aeson, base, bytestring, bytestring-conversion
, containers, cryptonite, currency-codes, errors, exceptions, hpack
, imports, lens, lib, memory, QuickCheck, schema-profunctor
, string-conversions, tagged, tasty, tasty-hunit, tasty-quickcheck
, text, time, types-common, uuid, wire-api
}:
mkDerivation {
  pname = "galley-types";
  version = "0.81.0";
  src = /home/axeman/workspace/wire-server/libs/galley-types;
  libraryHaskellDepends = [
    aeson base bytestring bytestring-conversion containers cryptonite
    currency-codes errors exceptions imports lens memory QuickCheck
    schema-profunctor string-conversions tagged text time types-common
    uuid wire-api
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson base containers imports lens QuickCheck tasty tasty-hunit
    tasty-quickcheck types-common wire-api
  ];
  prePatch = "hpack";
  license = lib.licenses.agpl3Only;
}
