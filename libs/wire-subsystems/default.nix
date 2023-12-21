# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, base
, bilge
, bytestring
, containers
, gitignoreSource
, gundeck-types
, hspec
, hspec-discover
, http-client
, http-types
, imports
, lens
, lib
, polysemy
, polysemy-wire-zoo
, QuickCheck
, quickcheck-instances
, text
, types-common
, wire-api
}:
mkDerivation {
  pname = "wire-subsystems";
  version = "0.1.0";
  src = gitignoreSource ./.;
  libraryHaskellDepends = [
    aeson
    base
    bilge
    containers
    gundeck-types
    http-client
    http-types
    imports
    lens
    polysemy
    polysemy-wire-zoo
    QuickCheck
    text
    types-common
    wire-api
  ];
  testHaskellDepends = [
    aeson
    base
    bytestring
    containers
    gundeck-types
    hspec
    imports
    polysemy
    polysemy-wire-zoo
    QuickCheck
    quickcheck-instances
    types-common
    wire-api
  ];
  testToolDepends = [ hspec-discover ];
  license = lib.licenses.agpl3Only;
}
