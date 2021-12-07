{ mkDerivation, array, base, bifunctors, comonad, containers
, contravariant, ghc-prim, hspec, hspec-discover, lib, profunctors
, QuickCheck, StateVar, stm, tagged, template-haskell
, th-abstraction, transformers, transformers-compat
, unordered-containers
}:
mkDerivation {
  pname = "invariant";
  version = "0.5.3";
  sha256 = "d73e5def38da9fdd85def073857aa5f4b1d3b0c2df05c43d58a677cca02d440c";
  revision = "2";
  editedCabalFile = "0vsil8x0z283n4993nk9m0v0y0za1b6lph59k1rb2i4wj05syx2v";
  libraryHaskellDepends = [
    array base bifunctors comonad containers contravariant ghc-prim
    profunctors StateVar stm tagged template-haskell th-abstraction
    transformers transformers-compat unordered-containers
  ];
  testHaskellDepends = [ base hspec QuickCheck template-haskell ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/nfrisby/invariant-functors";
  description = "Haskell98 invariant functors";
  license = lib.licenses.bsd2;
}
