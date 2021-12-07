{ mkDerivation, array, base, comonad, containers, contravariant
, distributive, free, generic-deriving, hspec, hspec-discover, lib
, mtl, profunctors, semigroupoids, semigroups, tagged, transformers
, transformers-compat, void
}:
mkDerivation {
  pname = "adjunctions";
  version = "4.4";
  sha256 = "507c2ef55337ae61c805f8cbc1213dfd7d2b85187342675d662254b8d8a16ae9";
  revision = "2";
  editedCabalFile = "1yfsjx7dqikg3hvld7i91xfsg5lawmr5980lvfd794sybmgxsf17";
  libraryHaskellDepends = [
    array base comonad containers contravariant distributive free mtl
    profunctors semigroupoids semigroups tagged transformers
    transformers-compat void
  ];
  testHaskellDepends = [ base distributive generic-deriving hspec ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://github.com/ekmett/adjunctions/";
  description = "Adjunctions and representable functors";
  license = lib.licenses.bsd3;
}
