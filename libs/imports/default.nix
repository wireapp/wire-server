# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, base
, bytestring
, containers
, deepseq
, extra
, gitignoreSource
, lib
, mtl
, text
, transformers
, unliftio
, unliftio-core
, unordered-containers
}:
mkDerivation {
  pname = "imports";
  version = "0.1.0";
  src = gitignoreSource ./.;
  libraryHaskellDepends = [
    base
    bytestring
    containers
    deepseq
    extra
    mtl
    text
    transformers
    unliftio
    unliftio-core
    unordered-containers
  ];
  description = "Very common imports";
  license = lib.licenses.agpl3Only;
}
