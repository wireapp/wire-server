{ mkDerivation, base, bytestring, criterion, deepseq, ghc-prim
, HUnit, integer-gmp, lib, QuickCheck, random, siphash
, test-framework, test-framework-hunit, test-framework-quickcheck2
, text, unix
}:
mkDerivation {
  pname = "hashable";
  version = "1.3.0.0";
  sha256 = "822e5413fbccca6ae884d3aba4066422c8b5d58d23d18b9ecb5c03273bb19ab4";
  revision = "2";
  editedCabalFile = "16va8hx4ynw0n5s2warhs13ilj7hrs5fcdn140h1fiix480as36n";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring deepseq ghc-prim integer-gmp text
  ];
  testHaskellDepends = [
    base bytestring ghc-prim HUnit QuickCheck random test-framework
    test-framework-hunit test-framework-quickcheck2 text unix
  ];
  benchmarkHaskellDepends = [
    base bytestring criterion ghc-prim integer-gmp siphash text
  ];
  homepage = "http://github.com/tibbe/hashable";
  description = "A class for types that can be converted to a hash value";
  license = lib.licenses.bsd3;
}
