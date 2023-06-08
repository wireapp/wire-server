# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, amqp
, brig
, cassandra-util
, exceptions
, extended
, federator
, galley
, galley-types
, gitignoreSource
, HsOpenSSL
, hspec
, http-client
, http2-manager
, imports
, lib
, monad-control
, polysemy
, polysemy-wire-zoo
, QuickCheck
, retry
, text
, tinylog
, transformers
, transformers-base
, types-common
, wire-api
, wire-api-federation
}:
mkDerivation {
  pname = "background-worker";
  version = "0.1.0.0";
  src = gitignoreSource ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    amqp
    brig
    cassandra-util
    exceptions
    extended
    galley
    galley-types
    HsOpenSSL
    http-client
    http2-manager
    imports
    monad-control
    polysemy
    polysemy-wire-zoo
    retry
    text
    tinylog
    transformers
    transformers-base
    types-common
    wire-api-federation
  ];
  executableHaskellDepends = [ HsOpenSSL imports types-common ];
  testHaskellDepends = [
    aeson
    amqp
    cassandra-util
    federator
    hspec
    http-client
    imports
    QuickCheck
    tinylog
    types-common
    wire-api
    wire-api-federation
  ];
  description = "Runs background work";
  license = lib.licenses.agpl3Only;
  mainProgram = "background-worker";
}
