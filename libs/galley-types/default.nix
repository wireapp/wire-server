# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation, aeson, base, bytestring, bytestring-conversion
, containers, cryptonite, currency-codes, errors, exceptions
, gitignoreSource, imports, lens, lib, memory, QuickCheck
, schema-profunctor, string-conversions, swagger2, tagged, tasty
, tasty-hunit, tasty-quickcheck, text, time, types-common, uuid
, wire-api
}:
mkDerivation {
  pname = "galley-types";
  version = "0.81.0";
  src = gitignoreSource ./.;
  libraryHaskellDepends = [
    aeson base bytestring bytestring-conversion containers cryptonite
    currency-codes errors exceptions imports lens memory QuickCheck
    schema-profunctor string-conversions swagger2 tagged text time
    types-common uuid wire-api
  ];
  testHaskellDepends = [
    aeson base containers imports lens QuickCheck tasty tasty-hunit
    tasty-quickcheck types-common wire-api
  ];
  license = lib.licenses.agpl3Only;
}
