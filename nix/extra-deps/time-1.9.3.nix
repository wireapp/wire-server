{ mkDerivation, base, deepseq, lib, QuickCheck, random, tasty
, tasty-hunit, tasty-quickcheck, unix
}:
mkDerivation {
  pname = "time";
  version = "1.9.3";
  sha256 = "575b967eacff3f36075dffc03f8b1908de3062595f894d8fe43b367f95a6c70e";
  libraryHaskellDepends = [ base deepseq ];
  testHaskellDepends = [
    base deepseq QuickCheck random tasty tasty-hunit tasty-quickcheck
    unix
  ];
  homepage = "https://github.com/haskell/time";
  description = "A time library";
  license = lib.licenses.bsd3;
}
