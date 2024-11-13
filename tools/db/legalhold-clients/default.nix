# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, base
, cassandra-util
, conduit
, cql
, gitignoreSource
, imports
, lens
, lib
, optparse-applicative
, time
, tinylog
, types-common
}:
mkDerivation {
  pname = "legalhold-clients";
  version = "1.0.0";
  src = gitignoreSource ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    cassandra-util
    conduit
    cql
    imports
    lens
    optparse-applicative
    time
    tinylog
    types-common
  ];
  executableHaskellDepends = [ base ];
  description = "get legalhold client info from cassandra";
  license = lib.licenses.agpl3Only;
  mainProgram = "legalhold-clients";
}
