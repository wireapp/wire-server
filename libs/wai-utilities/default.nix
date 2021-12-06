{ mkDerivation, aeson, async, base, bytestring
, bytestring-conversion, errors, exceptions, hpack, http-types
, imports, lib, metrics-core, metrics-wai, pipes, prometheus-client
, streaming-commons, string-conversions, swagger, text, tinylog
, types-common, unix, wai, wai-predicates, wai-routing, warp
}:
mkDerivation {
  pname = "wai-utilities";
  version = "0.16.1";
  src = /home/axeman/workspace/wire-server/libs/wai-utilities;
  libraryHaskellDepends = [
    aeson async base bytestring bytestring-conversion errors exceptions
    http-types imports metrics-core metrics-wai pipes prometheus-client
    streaming-commons string-conversions swagger text tinylog
    types-common unix wai wai-predicates wai-routing warp
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  description = "Various helpers for WAI";
  license = lib.licenses.agpl3Only;
}
