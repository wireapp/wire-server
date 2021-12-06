{ mkDerivation, aeson, aeson-qq, base, bifunctors, comonad
, containers, hpack, imports, insert-ordered-containers, lens, lib
, profunctors, swagger2, tasty, tasty-hunit, text, transformers
, vector
}:
mkDerivation {
  pname = "schema-profunctor";
  version = "0.1.0";
  src = /home/axeman/workspace/wire-server/libs/schema-profunctor;
  libraryHaskellDepends = [
    aeson base bifunctors comonad containers imports lens profunctors
    swagger2 text transformers vector
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson aeson-qq base imports insert-ordered-containers lens swagger2
    tasty tasty-hunit text
  ];
  prePatch = "hpack";
  license = lib.licenses.agpl3Only;
}
