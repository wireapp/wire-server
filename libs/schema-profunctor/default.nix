# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation, aeson, aeson-qq, base, bifunctors, comonad
, containers, gitignoreSource, imports, insert-ordered-containers
, lens, lib, profunctors, swagger2, tasty, tasty-hunit, text
, transformers, vector
}:
mkDerivation {
  pname = "schema-profunctor";
  version = "0.1.0";
  src = gitignoreSource ./.;
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
