{ mkDerivation, base, Cabal, containers, directory, filepath
, ghc-boot-th, lib, mtl, pretty, process, syb, tasty, tasty-golden
, template-haskell, text, th-desugar, transformers, turtle
}:
mkDerivation {
  pname = "singletons";
  version = "2.6";
  sha256 = "bea771f7017ad35413215cbbd275a19c3d2a338c92e37b0bb64402385cb886d1";
  setupHaskellDepends = [ base Cabal directory filepath ];
  libraryHaskellDepends = [
    base containers ghc-boot-th mtl pretty syb template-haskell text
    th-desugar transformers
  ];
  testHaskellDepends = [
    base filepath process tasty tasty-golden text turtle
  ];
  homepage = "http://www.github.com/goldfirere/singletons";
  description = "A framework for generating singleton types";
  license = lib.licenses.bsd3;
}
