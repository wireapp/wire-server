{ mkDerivation, base, criterion, deepseq, ghc-prim, lib, sop-core
, template-haskell, th-abstraction
}:
mkDerivation {
  pname = "generics-sop";
  version = "0.5.1.0";
  sha256 = "eac657aa743282a81e03438744ac93053d989b829f6b3ca4837c70bbca041f3c";
  revision = "1";
  editedCabalFile = "1m61bb6k96ybsrc3hpxn0fdspq9mbkyfklx7vfnd55mava4ahzp2";
  libraryHaskellDepends = [
    base ghc-prim sop-core template-haskell th-abstraction
  ];
  testHaskellDepends = [ base ];
  benchmarkHaskellDepends = [
    base criterion deepseq template-haskell
  ];
  description = "Generic Programming using True Sums of Products";
  license = lib.licenses.bsd3;
}
