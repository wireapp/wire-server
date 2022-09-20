{ mkDerivation, aeson, aeson-qq, base, bifunctors, comonad
, containers, imports, insert-ordered-containers, lens, lib
, profunctors, swagger2, tasty, tasty-hunit, text, transformers
, vector
}:
mkDerivation {
  pname = "schema-profunctor";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bifunctors comonad containers imports lens profunctors
    swagger2 text transformers vector
  ];
  testHaskellDepends = [
    aeson aeson-qq base imports insert-ordered-containers lens swagger2
    tasty tasty-hunit text
  ];
  license = lib.licenses.agpl3Only;
}
