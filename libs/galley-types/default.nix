{ mkDerivation, aeson, base, bytestring, bytestring-conversion
, containers, cryptonite, currency-codes, errors, exceptions
, imports, lens, lib, memory, QuickCheck, schema-profunctor
, string-conversions, tagged, tasty, tasty-hunit, tasty-quickcheck
, text, time, types-common, uuid, wire-api
}:
mkDerivation {
  pname = "galley-types";
  version = "0.81.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring bytestring-conversion containers cryptonite
    currency-codes errors exceptions imports lens memory QuickCheck
    schema-profunctor string-conversions tagged text time types-common
    uuid wire-api
  ];
  testHaskellDepends = [
    aeson base containers imports lens QuickCheck tasty tasty-hunit
    tasty-quickcheck types-common wire-api
  ];
  license = lib.licenses.agpl3Only;
}
