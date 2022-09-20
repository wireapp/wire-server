{ mkDerivation, attoparsec, base, base64-bytestring, bytestring
, bytestring-conversion, errors, exceptions, imports, lens, lib
, mtl, mwc-random, optparse-applicative, sodium-crypto-sign, tasty
, tasty-hunit, tasty-quickcheck, time, uuid, vector
}:
mkDerivation {
  pname = "zauth";
  version = "0.10.3";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base base64-bytestring bytestring bytestring-conversion
    errors exceptions imports lens mtl mwc-random sodium-crypto-sign
    time uuid vector
  ];
  executableHaskellDepends = [
    base base64-bytestring bytestring bytestring-conversion errors
    imports lens optparse-applicative sodium-crypto-sign uuid
  ];
  testHaskellDepends = [
    base bytestring bytestring-conversion errors imports lens
    sodium-crypto-sign tasty tasty-hunit tasty-quickcheck uuid
  ];
  description = "Creation and validation of signed tokens";
  license = lib.licenses.agpl3Only;
  mainProgram = "zauth";
}
