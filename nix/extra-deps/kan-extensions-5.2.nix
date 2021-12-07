{ mkDerivation, adjunctions, array, base, comonad, containers
, contravariant, distributive, free, invariant, lib, mtl
, profunctors, semigroupoids, tagged, transformers
, transformers-compat
}:
mkDerivation {
  pname = "kan-extensions";
  version = "5.2";
  sha256 = "6b727e586f744b96529415eeabc745dfe05feea61f6b6bad90c224c879f4dbd3";
  revision = "1";
  editedCabalFile = "1kiazy9sd42iham8h9f6biifiwc26x0fci4p0376wq1n6fcxl9y4";
  libraryHaskellDepends = [
    adjunctions array base comonad containers contravariant
    distributive free invariant mtl profunctors semigroupoids tagged
    transformers transformers-compat
  ];
  homepage = "http://github.com/ekmett/kan-extensions/";
  description = "Kan extensions, Kan lifts, the Yoneda lemma, and (co)density (co)monads";
  license = lib.licenses.bsd3;
}
