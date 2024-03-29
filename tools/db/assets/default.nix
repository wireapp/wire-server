# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, attoparsec
, base
, bytestring-conversion
, cassandra-util
, conduit
, gitignoreSource
, imports
, lens
, lib
, optparse-applicative
, text
, tinylog
, types-common
, wire-api
}:
mkDerivation {
  pname = "assets";
  version = "1.0.0";
  src = gitignoreSource ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec
    base
    bytestring-conversion
    cassandra-util
    conduit
    imports
    lens
    optparse-applicative
    text
    tinylog
    types-common
    wire-api
  ];
  executableHaskellDepends = [ base ];
  description = "Scan the brig user table, search for malformatted asset keys and print them";
  license = lib.licenses.agpl3Only;
  mainProgram = "assets";
}
