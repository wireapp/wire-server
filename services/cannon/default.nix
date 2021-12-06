{ mkDerivation, aeson, api-field-json-th, async, base, bilge
, bytestring, bytestring-conversion, criterion, data-default
, data-timeout, exceptions, extended, gundeck-types, hashable
, hpack, http-types, imports, lens, lens-family-core, lib
, metrics-wai, mwc-random, QuickCheck, random, retry
, safe-exceptions, strict, swagger, tasty, tasty-hunit
, tasty-quickcheck, text, tinylog, types-common, uuid, vector, wai
, wai-extra, wai-predicates, wai-routing, wai-utilities
, wai-websockets, warp, websockets
}:
mkDerivation {
  pname = "cannon";
  version = "0.31.0";
  src = /home/axeman/workspace/wire-server/services/cannon;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson api-field-json-th async base bilge bytestring
    bytestring-conversion data-default data-timeout exceptions extended
    gundeck-types hashable http-types imports lens lens-family-core
    metrics-wai mwc-random retry safe-exceptions strict swagger text
    tinylog types-common uuid vector wai wai-extra wai-predicates
    wai-routing wai-utilities wai-websockets warp websockets
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base extended imports types-common ];
  testHaskellDepends = [
    async base bytestring criterion extended imports metrics-wai
    QuickCheck random tasty tasty-hunit tasty-quickcheck types-common
    uuid wai-utilities
  ];
  prePatch = "hpack";
  description = "Push Notification API";
  license = lib.licenses.agpl3Only;
}
