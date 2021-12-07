{ mkDerivation, array, base, containers, criterion, deepseq
, fingertree-psqueue, ghc-prim, hashable, HUnit, lib, mtl, PSQueue
, QuickCheck, random, tagged, tasty, tasty-hunit, tasty-quickcheck
, unordered-containers
}:
mkDerivation {
  pname = "psqueues";
  version = "0.2.7.2";
  sha256 = "26263b555d943f9b18bbebda6a090848fdba3c1b403a9b7c848f6bac99e893f9";
  revision = "1";
  editedCabalFile = "0d0mm3c8x31dasfzp1884r2irkm3c9irvvbahjzfr1bzzxfb7vyv";
  libraryHaskellDepends = [ base deepseq ghc-prim hashable ];
  testHaskellDepends = [
    array base deepseq ghc-prim hashable HUnit QuickCheck tagged tasty
    tasty-hunit tasty-quickcheck
  ];
  benchmarkHaskellDepends = [
    base containers criterion deepseq fingertree-psqueue ghc-prim
    hashable mtl PSQueue random unordered-containers
  ];
  description = "Pure priority search queues";
  license = lib.licenses.bsd3;
}
