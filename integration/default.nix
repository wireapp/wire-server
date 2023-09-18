# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, aeson-pretty
, array
, async
, attoparsec
, base
, base64-bytestring
, bytestring
, bytestring-conversion
, Cabal
, case-insensitive
, containers
, cql
, cql-io
, cryptonite
, data-default
, directory
, errors
, exceptions
, extended
, extra
, filepath
, gitignoreSource
, hex
, http-client
, http-types
, kan-extensions
, lens
, lens-aeson
, lib
, memory
, mime
, monad-control
, mtl
, network
, network-uri
, optparse-applicative
, process
, proto-lens
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
, transformers-base
, unix
, unliftio
, uuid
, vector
, websockets
, wire-message-proto-lens
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
    attoparsec
    base
    base64-bytestring
    bytestring
    bytestring-conversion
    case-insensitive
    containers
    cql
    cql-io
    cryptonite
    data-default
    directory
    errors
    exceptions
    extended
    extra
    filepath
    hex
    http-client
    http-types
    kan-extensions
    lens
    lens-aeson
    memory
    mime
    monad-control
    mtl
    network
    network-uri
    optparse-applicative
    process
    proto-lens
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
    transformers-base
    unix
    unliftio
    uuid
    vector
    websockets
    wire-message-proto-lens
    yaml
  ];
  license = lib.licenses.agpl3Only;
}
