# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, aeson-pretty
, array
, async
, base
, bytestring
, Cabal
, case-insensitive
, containers
, data-default
, directory
, filepath
, gitignoreSource
, http-client
, http-types
, imports
, lib
, network
, network-uri
, process
, random
, raw-strings-qq
, retry
, scientific
, split
, tagged
, tasty
, text
, transformers
, yaml
}:
mkDerivation {
  pname = "integration";
  version = "0.1.0";
  src = gitignoreSource ./.;
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal containers directory filepath ];
  libraryHaskellDepends = [
    aeson
    aeson-pretty
    array
    async
    base
    bytestring
    case-insensitive
    containers
    data-default
    filepath
    http-client
    http-types
    imports
    network
    network-uri
    process
    random
    retry
    scientific
    split
    tagged
    tasty
    text
    transformers
    yaml
  ];
  testHaskellDepends = [ aeson base imports raw-strings-qq tasty ];
  license = lib.licenses.agpl3Only;
  mainProgram = "integration";
}
