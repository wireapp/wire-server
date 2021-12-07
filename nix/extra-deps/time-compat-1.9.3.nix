{ mkDerivation, base, base-compat, base-orphans, deepseq, HUnit
, lib, QuickCheck, tagged, tasty, tasty-hunit, tasty-quickcheck
, time
}:
mkDerivation {
  pname = "time-compat";
  version = "1.9.3";
  sha256 = "bb6a44e667945ddca8ded93e041ee91986a4a19f59e149a5dd21fdb1bfa3db88";
  libraryHaskellDepends = [ base base-orphans deepseq time ];
  testHaskellDepends = [
    base base-compat deepseq HUnit QuickCheck tagged tasty tasty-hunit
    tasty-quickcheck time
  ];
  homepage = "https://github.com/phadej/time-compat";
  description = "Compatibility package for time";
  license = lib.licenses.bsd3;
}
