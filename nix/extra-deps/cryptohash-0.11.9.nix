{ mkDerivation, base, byteable, bytestring, criterion, cryptonite
, ghc-prim, HUnit, lib, memory, QuickCheck, tasty, tasty-hunit
, tasty-quickcheck
}:
mkDerivation {
  pname = "cryptohash";
  version = "0.11.9";
  sha256 = "c28f847fc1fcd65b6eea2e74a100300af940919f04bb21d391f6a773968f22fb";
  libraryHaskellDepends = [
    base byteable bytestring cryptonite ghc-prim memory
  ];
  testHaskellDepends = [
    base byteable bytestring HUnit QuickCheck tasty tasty-hunit
    tasty-quickcheck
  ];
  benchmarkHaskellDepends = [ base byteable bytestring criterion ];
  homepage = "http://github.com/vincenthz/hs-cryptohash";
  description = "collection of crypto hashes, fast, pure and practical";
  license = lib.licenses.bsd3;
}
