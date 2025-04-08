# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, amqp
, async
, base
, bytestring
, bytestring-conversion
, cassandra-util
, containers
, data-default
, exceptions
, extended
, federator
, gitignoreSource
, HsOpenSSL
, hspec
, http-client
, http-media
, http-types
, http2-manager
, imports
, kan-extensions
, lib
, metrics-wai
, monad-control
, prometheus-client
, QuickCheck
, retry
, servant
, servant-client
, servant-client-core
, servant-server
, text
, tinylog
, transformers
, transformers-base
, types-common
, unliftio
, utf8-string
, wai
, wai-utilities
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
    async
    base
    bytestring
    bytestring-conversion
    cassandra-util
    containers
    exceptions
    extended
    HsOpenSSL
    http-client
    http2-manager
    imports
    kan-extensions
    metrics-wai
    monad-control
    prometheus-client
    retry
    servant-client
    servant-server
    text
    tinylog
    transformers
    transformers-base
    types-common
    unliftio
    utf8-string
    wai-utilities
    wire-api
    wire-api-federation
  ];
  executableHaskellDepends = [ HsOpenSSL imports types-common ];
  testHaskellDepends = [
    aeson
    amqp
    base
    bytestring
    containers
    data-default
    extended
    federator
    hspec
    http-client
    http-media
    http-types
    imports
    prometheus-client
    QuickCheck
    servant
    servant-client
    servant-client-core
    servant-server
    text
    tinylog
    transformers
    types-common
    unliftio
    wai
    wai-utilities
    wire-api
    wire-api-federation
  ];
  description = "Runs background work";
  license = lib.licenses.agpl3Only;
  mainProgram = "background-worker";
}
