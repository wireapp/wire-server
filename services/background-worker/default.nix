# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, amqp
, async
, base
, bilge
, bytestring
, bytestring-conversion
, containers
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
, HUnit
, imports
, lens
, lib
, metrics-core
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
    bilge
    bytestring
    bytestring-conversion
    containers
    exceptions
    extended
    HsOpenSSL
    http-client
    http-types
    http2-manager
    imports
    lens
    metrics-core
    metrics-wai
    monad-control
    prometheus-client
    retry
    servant-client
    servant-server
    text
    tinylog
    transformers-base
    types-common
    wire-api
    wire-api-federation
  ];
  executableHaskellDepends = [
    aeson
    amqp
    base
    bytestring
    containers
    extended
    federator
    HsOpenSSL
    hspec
    http-client
    http-media
    http-types
    HUnit
    imports
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
    wire-api
    unliftio
    wai-utilities
    wire-api-federation
  ];
  testHaskellDepends = [
    aeson
    amqp
    base
    bytestring
    containers
    extended
    federator
    hspec
    http-client
    http-media
    http-types
    HUnit
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
    wire-api
    wire-api-federation
  ];
  description = "Runs background work";
  license = lib.licenses.agpl3Only;
}
