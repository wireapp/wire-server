{ mkDerivation, aeson, attoparsec, base, bytestring, Cabal
, cabal-doctest, doctest, generic-deriving, lens, lib, scientific
, semigroups, simple-reflect, text, unordered-containers, vector
}:
mkDerivation {
  pname = "lens-aeson";
  version = "1.1";
  sha256 = "f7bc9c6f95735b523afac6316195d06b31f9b85c84918960096e4eecdb6cc90e";
  revision = "4";
  editedCabalFile = "1wgk0nd0fxgdbqb6mkslj3gyrs9vdxpb83hvj2n2dcswg3ahwdsy";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    aeson attoparsec base bytestring lens scientific text
    unordered-containers vector
  ];
  testHaskellDepends = [
    base doctest generic-deriving semigroups simple-reflect
  ];
  homepage = "http://github.com/lens/lens-aeson/";
  description = "Law-abiding lenses for aeson";
  license = lib.licenses.mit;
}
