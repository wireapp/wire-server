# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, attoparsec
, base
, base64-bytestring
, bytestring
, bytestring-conversion
, errors
, gitignoreSource
, imports
, lib
, optparse-applicative
, sodium-crypto-sign
, tasty
, tasty-hunit
, tasty-quickcheck
, text
, time
, uuid
, vector
}:
mkDerivation {
  pname = "zauth";
  version = "0.10.3";
  src = gitignoreSource ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec
    base
    base64-bytestring
    bytestring
    bytestring-conversion
    errors
    imports
    sodium-crypto-sign
    time
    uuid
    vector
  ];
  executableHaskellDepends = [
    base
    imports
    optparse-applicative
    sodium-crypto-sign
  ];
  testHaskellDepends = [
    base
    bytestring-conversion
    imports
    sodium-crypto-sign
    tasty
    tasty-hunit
    tasty-quickcheck
    text
    uuid
    vector
  ];
  description = "Creation and validation of signed tokens";
  license = lib.licenses.agpl3Only;
  mainProgram = "zauth";
}
