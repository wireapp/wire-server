{ mkDerivation, base, byteorder, deepseq, directory, dlist
, ghc-prim, HUnit, integer-gmp, lib, mtl, QuickCheck, random
, test-framework, test-framework-hunit, test-framework-quickcheck2
}:
mkDerivation {
  pname = "bytestring";
  version = "0.10.10.1";
  sha256 = "7867759c00d0635709c646c929af70b5510a6cd2216e6671484fc9c23df2acc6";
  revision = "1";
  editedCabalFile = "0qkxs8crh8g60k5k7bzria9vrqnjzl0229rjbc1vrfqphlddgdp5";
  libraryHaskellDepends = [ base deepseq ghc-prim integer-gmp ];
  testHaskellDepends = [
    base byteorder deepseq directory dlist ghc-prim HUnit mtl
    QuickCheck random test-framework test-framework-hunit
    test-framework-quickcheck2
  ];
  homepage = "https://github.com/haskell/bytestring";
  description = "Fast, compact, strict and lazy byte strings with a list interface";
  license = lib.licenses.bsd3;
}
