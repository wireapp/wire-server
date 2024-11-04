# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, async
, base
, bilge
, bytestring
, bytestring-conversion
, conduit
, criterion
, data-timeout
, exceptions
, extended
, extra
, gitignoreSource
, hashable
, hs-opentelemetry-instrumentation-wai
, hs-opentelemetry-sdk
, http-types
, imports
, lens
, lens-family-core
, lib
, metrics-wai
, mwc-random
, prometheus-client
, QuickCheck
, random
, retry
, safe-exceptions
, servant-conduit
, servant-server
, strict
, tasty
, tasty-hunit
, tasty-quickcheck
, text
, tinylog
, types-common
, unix
, unliftio
, uuid
, vector
, wai
, wai-extra
, wai-utilities
, warp
, websockets
, wire-api
, wire-otel
, wire-subsystems
}:
mkDerivation {
  pname = "cannon";
  version = "0.31.0";
  src = gitignoreSource ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    async
    base
    bilge
    bytestring
    bytestring-conversion
    conduit
    data-timeout
    exceptions
    extended
    extra
    hashable
    hs-opentelemetry-instrumentation-wai
    hs-opentelemetry-sdk
    http-types
    imports
    lens
    lens-family-core
    metrics-wai
    mwc-random
    prometheus-client
    retry
    safe-exceptions
    servant-conduit
    servant-server
    strict
    text
    tinylog
    types-common
    unix
    unliftio
    vector
    wai
    wai-extra
    wai-utilities
    warp
    websockets
    wire-api
    wire-otel
    wire-subsystems
  ];
  executableHaskellDepends = [ base imports types-common ];
  testHaskellDepends = [
    async
    base
    bytestring
    imports
    metrics-wai
    QuickCheck
    random
    tasty
    tasty-hunit
    tasty-quickcheck
    uuid
    wire-api
  ];
  benchmarkHaskellDepends = [ async base criterion imports uuid ];
  description = "Push Notification API";
  license = lib.licenses.agpl3Only;
  mainProgram = "cannon";
}
