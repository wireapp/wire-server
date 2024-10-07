# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, base
, bloodhound
, bytestring
, gitignoreSource
, http-types
, imports
, lib
, text
}:
mkDerivation {
  pname = "es-compat";
  version = "0.1.0.0";
  src = gitignoreSource ./.;
  libraryHaskellDepends = [
    aeson
    base
    bloodhound
    bytestring
    http-types
    imports
    text
  ];
  description = "ElasticSearch 6 to 8 compatibility layer";
  license = lib.licenses.agpl3Plus;
}
