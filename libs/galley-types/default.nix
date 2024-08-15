# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, base
, bytestring
, bytestring-conversion
, containers
, crypton
, data-default
, errors
, gitignoreSource
, imports
, lens
, lib
, memory
, QuickCheck
, sop-core
, text
, types-common
, utf8-string
, uuid
, wire-api
}:
mkDerivation {
  pname = "galley-types";
  version = "0.81.0";
  src = gitignoreSource ./.;
  libraryHaskellDepends = [
    aeson
    base
    bytestring
    bytestring-conversion
    containers
    crypton
    data-default
    errors
    imports
    lens
    memory
    QuickCheck
    sop-core
    text
    types-common
    utf8-string
    uuid
    wire-api
  ];
  license = lib.licenses.agpl3Only;
}
