{ mkDerivation, base, Cabal, deepseq, lib, QuickCheck }:
mkDerivation {
  pname = "dlist";
  version = "0.8.0.8";
  sha256 = "7129cf18068d3384e305708a10426ab8f573bee1030b023a114f45f1d0ec496d";
  libraryHaskellDepends = [ base deepseq ];
  testHaskellDepends = [ base Cabal QuickCheck ];
  homepage = "https://github.com/spl/dlist";
  description = "Difference lists";
  license = lib.licenses.bsd3;
}
