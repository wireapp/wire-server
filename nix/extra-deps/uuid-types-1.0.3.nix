{ mkDerivation, base, binary, bytestring, containers, criterion
, deepseq, hashable, HUnit, lib, QuickCheck, random, tasty
, tasty-hunit, tasty-quickcheck, text
}:
mkDerivation {
  pname = "uuid-types";
  version = "1.0.3";
  sha256 = "9276517ab24a9b06f39d6e3c33c6c2b4ace1fc2126dbc1cd9806866a6551b3fd";
  revision = "4";
  editedCabalFile = "0ipwfd5y8021ygpadjjhchw5irm0x27dlv1k2hf4znza5v7hadcn";
  libraryHaskellDepends = [
    base binary bytestring deepseq hashable random text
  ];
  testHaskellDepends = [
    base bytestring HUnit QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  benchmarkHaskellDepends = [
    base bytestring containers criterion deepseq random
  ];
  homepage = "https://github.com/aslatter/uuid";
  description = "Type definitions for Universally Unique Identifiers";
  license = lib.licenses.bsd3;
}
