{ mkDerivation, array, base, bytestring, containers, ghc-prim, lib
, QuickCheck, test-framework, test-framework-quickcheck2
}:
mkDerivation {
  pname = "cereal";
  version = "0.5.8.1";
  sha256 = "2d9e88ac934b9ebc058097c72011ff59f3f146176310e1c957a0e4cf63681bd7";
  revision = "1";
  editedCabalFile = "02v1nivac478nzzykjbq3rawnqskxjc4sb54m6s5jcgbigfnb2x0";
  libraryHaskellDepends = [
    array base bytestring containers ghc-prim
  ];
  testHaskellDepends = [
    base bytestring QuickCheck test-framework
    test-framework-quickcheck2
  ];
  homepage = "https://github.com/GaloisInc/cereal";
  description = "A binary serialization library";
  license = lib.licenses.bsd3;
}
