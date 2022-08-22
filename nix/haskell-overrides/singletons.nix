{ mkDerivation, base, bytestring, Cabal, containers, deepseq
, directory, filepath, ghc-boot-th, lib, mtl, pretty, process, syb
, tasty, tasty-golden, template-haskell, text, th-desugar
, transformers, turtle
}:
mkDerivation {
  pname = "singletons";
  version = "2.7";
  sha256 = "e12bd6e695eaf444eb6b1fd07372818aaff8703aa71265f677f3af3cb412e22b";
  revision = "2";
  editedCabalFile = "0ar9prm5s0y4shsskn0mlkdhsiiagppv3nppmv16513201di0rd2";
  setupHaskellDepends = [ base Cabal directory filepath ];
  libraryHaskellDepends = [
    base containers ghc-boot-th mtl pretty syb template-haskell text
    th-desugar transformers
  ];
  testHaskellDepends = [
    base bytestring deepseq filepath process tasty tasty-golden text
    turtle
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://www.github.com/goldfirere/singletons";
  description = "A framework for generating singleton types";
  license = lib.licenses.bsd3;
}
