{ mkDerivation, base, criterion, deepseq, ghc-prim, lib, QuickCheck
}:
mkDerivation {
  pname = "pretty";
  version = "1.1.3.6";
  sha256 = "eb12cc23cbaf7d3f3e2a5e804e2e0ddb2a2d98d3ad456617d1b87fd3951d66e8";
  libraryHaskellDepends = [ base deepseq ghc-prim ];
  testHaskellDepends = [ base deepseq ghc-prim QuickCheck ];
  benchmarkHaskellDepends = [ base criterion ];
  homepage = "http://github.com/haskell/pretty";
  description = "Pretty-printing library";
  license = lib.licenses.bsd3;
}
