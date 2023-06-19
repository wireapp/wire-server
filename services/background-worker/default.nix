# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, amqp
, containers
, exceptions
, extended
, federator
, gitignoreSource
, HsOpenSSL
, hspec
, http2-manager
, imports
, lib
, monad-control
, QuickCheck
, retry
, servant-client
, text
, tinylog
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
    containers
    exceptions
    extended
    HsOpenSSL
    http2-manager
    imports
    monad-control
    retry
    servant-client
    text
    tinylog
    transformers-base
    types-common
    wire-api-federation
  ];
  executableHaskellDepends = [ HsOpenSSL imports types-common ];
  testHaskellDepends = [
    aeson
    amqp
    federator
    hspec
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
