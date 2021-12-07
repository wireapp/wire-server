{ mkDerivation, array, base, binary, bytestring, deepseq, directory
, ghc-prim, HUnit, integer-gmp, lib, QuickCheck, quickcheck-unicode
, random, template-haskell, test-framework, test-framework-hunit
, test-framework-quickcheck2
}:
mkDerivation {
  pname = "text";
  version = "1.2.4.0";
  sha256 = "cae2a2c9adc9500f2554797fd501f8c8280cc32e16c9e293007465a9414ce34c";
  libraryHaskellDepends = [
    array base binary bytestring deepseq ghc-prim integer-gmp
    template-haskell
  ];
  testHaskellDepends = [
    array base binary bytestring deepseq directory ghc-prim HUnit
    integer-gmp QuickCheck quickcheck-unicode random template-haskell
    test-framework test-framework-hunit test-framework-quickcheck2
  ];
  doCheck = false;
  homepage = "https://github.com/haskell/text";
  description = "An efficient packed Unicode text type";
  license = lib.licenses.bsd2;
}
