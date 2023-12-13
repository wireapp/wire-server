# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, api-field-json-th
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
, gundeck-types
, hashable
, http-types
, imports
, lens
, lens-family-core
, lib
, metrics-wai
, mwc-random
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
}:
mkDerivation {
  pname = "cannon";
  version = "0.31.0";
  src = gitignoreSource ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    api-field-json-th
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
    gundeck-types
    hashable
    http-types
    imports
    lens
    lens-family-core
    metrics-wai
    mwc-random
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
