{ mkDerivation, base, deepseq, lib, QuickCheck }:
mkDerivation {
  pname = "safe";
  version = "0.3.19";
  sha256 = "25043442c8f8aa95955bb17467d023630632b961aaa61e807e325d9b2c33f7a2";
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base deepseq QuickCheck ];
  homepage = "https://github.com/ndmitchell/safe#readme";
  description = "Library of safe (exception free) functions";
  license = lib.licenses.bsd3;
}
