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
, base64-bytestring
, bytestring
, bytestring-conversion
, Cabal
, case-insensitive
, containers
, data-default
, directory
, exceptions
, extra
, filepath
, gitignoreSource
, hex
, http-client
, http-types
, kan-extensions
, lib
, mtl
, network
, network-uri
, optparse-applicative
, process
, random
, raw-strings-qq
, retry
, scientific
, split
, stm
, string-conversions
, tagged
, temporary
, text
, time
, transformers
, unix
, unliftio
, uuid
, websockets
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
    base64-bytestring
    bytestring
    bytestring-conversion
    case-insensitive
    containers
    data-default
    directory
    exceptions
    extra
    filepath
    hex
    http-client
    http-types
    kan-extensions
    mtl
    network
    network-uri
    optparse-applicative
    process
    random
    raw-strings-qq
    retry
    scientific
    split
    stm
    string-conversions
    tagged
    temporary
    text
    time
    transformers
    unix
    unliftio
    uuid
    websockets
    yaml
  ];
  license = lib.licenses.agpl3Only;
  mainProgram = "integration";
}
