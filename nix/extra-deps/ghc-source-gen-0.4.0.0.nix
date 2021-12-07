{ mkDerivation, base, ghc, ghc-paths, lib, QuickCheck, tasty
, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "ghc-source-gen";
  version = "0.4.0.0";
  sha256 = "906bfaf0ccdc99d1fd3204b885d2300ab47cc08e95b6d2c1df14d405a1b20332";
  libraryHaskellDepends = [ base ghc ];
  testHaskellDepends = [
    base ghc ghc-paths QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/google/ghc-source-gen#readme";
  description = "Constructs Haskell syntax trees for the GHC API";
  license = lib.licenses.bsd3;
}
