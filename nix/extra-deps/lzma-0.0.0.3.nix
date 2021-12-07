{ mkDerivation, base, bytestring, HUnit, lib, QuickCheck, tasty
, tasty-hunit, tasty-quickcheck, xz
}:
mkDerivation {
  pname = "lzma";
  version = "0.0.0.3";
  sha256 = "af8321c3511bde3e2745093fa3bd74c642e386db7d2e7c43b3a54814f1338144";
  revision = "7";
  editedCabalFile = "07gc1zl4a38p3yg7md1hfrb2ca7yjzansh1mv8xs0c89jaqbgcr5";
  libraryHaskellDepends = [ base bytestring ];
  librarySystemDepends = [ xz ];
  testHaskellDepends = [
    base bytestring HUnit QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/hvr/lzma";
  description = "LZMA/XZ compression and decompression";
  license = lib.licenses.bsd3;
}
