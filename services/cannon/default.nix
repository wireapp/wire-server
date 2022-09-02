# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation, aeson, api-field-json-th, async, base, bilge
, bytestring, bytestring-conversion, conduit, criterion
, data-default, data-timeout, exceptions, extended, extra
, gitignoreSource, gundeck-types, hashable, http-types, imports
, lens, lens-family-core, lib, metrics-wai, mwc-random, QuickCheck
, random, retry, safe-exceptions, servant, servant-conduit
, servant-server, strict, swagger, tasty, tasty-hunit
, tasty-quickcheck, text, tinylog, types-common, unix, unliftio
, uuid, vector, wai, wai-extra, wai-predicates, wai-utilities
, wai-websockets, warp, websockets, wire-api
}:
mkDerivation {
  pname = "cannon";
  version = "0.31.0";
  src = gitignoreSource ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson api-field-json-th async base bilge bytestring
    bytestring-conversion conduit data-default data-timeout exceptions
    extended extra gundeck-types hashable http-types imports lens
    lens-family-core metrics-wai mwc-random retry safe-exceptions
    servant servant-conduit servant-server strict swagger text tinylog
    types-common unix unliftio uuid vector wai wai-extra wai-predicates
    wai-utilities wai-websockets warp websockets wire-api
  ];
  executableHaskellDepends = [ base extended imports types-common ];
  testHaskellDepends = [
    async base bytestring criterion extended imports metrics-wai
    QuickCheck random tasty tasty-hunit tasty-quickcheck types-common
    uuid wai-utilities wire-api
  ];
  benchmarkHaskellDepends = [
    async base bytestring criterion extended imports metrics-wai
    QuickCheck random tasty tasty-hunit tasty-quickcheck types-common
    uuid wai-utilities
  ];
  description = "Push Notification API";
  license = lib.licenses.agpl3Only;
  mainProgram = "cannon";
}
