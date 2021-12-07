{ mkDerivation, base, containers, fail, ghc-prim, hspec, HUnit, lib
, mtl, ordered-containers, semigroups, syb, template-haskell
, th-abstraction, th-expand-syns, th-lift, th-orphans
, transformers-compat
}:
mkDerivation {
  pname = "th-desugar";
  version = "1.10";
  sha256 = "1bf5b113be50afbb416fb7c8540091ba68d5c5fb78693a490db45f8a8f207bbc";
  libraryHaskellDepends = [
    base containers fail ghc-prim mtl ordered-containers semigroups syb
    template-haskell th-abstraction th-lift th-orphans
    transformers-compat
  ];
  testHaskellDepends = [
    base containers hspec HUnit mtl syb template-haskell th-expand-syns
    th-lift th-orphans
  ];
  homepage = "https://github.com/goldfirere/th-desugar";
  description = "Functions to desugar Template Haskell";
  license = lib.licenses.bsd3;
}
