{ mkDerivation, base, containers, fail, ghc-prim, hspec, HUnit, lib
, mtl, ordered-containers, semigroups, syb, template-haskell
, th-abstraction, th-lift, th-orphans, transformers-compat
}:
mkDerivation {
  pname = "th-desugar";
  version = "1.11";
  sha256 = "14e29e035b96d7c35bb1503426736e610465f75939bd89df1386f2a0c26ce82a";
  revision = "1";
  editedCabalFile = "1gjg6vhlbxg7y2gx0frqh5arwqw0n3j777q6gk2p8f2k60gphifi";
  libraryHaskellDepends = [
    base containers fail ghc-prim mtl ordered-containers semigroups syb
    template-haskell th-abstraction th-lift th-orphans
    transformers-compat
  ];
  testHaskellDepends = [
    base containers hspec HUnit mtl syb template-haskell th-lift
    th-orphans
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/goldfirere/th-desugar";
  description = "Functions to desugar Template Haskell";
  license = lib.licenses.bsd3;
}
