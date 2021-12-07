{ mkDerivation, base, bytestring, ChasingBottoms, containers
, criterion, deepseq, deepseq-generics, hashable, hashmap, HUnit
, lib, mtl, QuickCheck, random, test-framework
, test-framework-hunit, test-framework-quickcheck2
}:
mkDerivation {
  pname = "unordered-containers";
  version = "0.2.10.0";
  sha256 = "65f117bdbdea9efc75fb9fd539873de7687e005d8898bb21821020a4b383c573";
  revision = "1";
  editedCabalFile = "01727lm2spbqfn0rl89zv9zhpsmnls0kmizyckqf5nc3j9d0k6sy";
  libraryHaskellDepends = [ base deepseq hashable ];
  testHaskellDepends = [
    base ChasingBottoms containers hashable HUnit QuickCheck
    test-framework test-framework-hunit test-framework-quickcheck2
  ];
  benchmarkHaskellDepends = [
    base bytestring containers criterion deepseq deepseq-generics
    hashable hashmap mtl random
  ];
  homepage = "https://github.com/tibbe/unordered-containers";
  description = "Efficient hashing-based container types";
  license = lib.licenses.bsd3;
}
