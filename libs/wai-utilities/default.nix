{ mkDerivation, aeson, async, base, bytestring
, bytestring-conversion, errors, exceptions, http-types, imports
, kan-extensions, lib, metrics-core, metrics-wai, pipes
, prometheus-client, streaming-commons, string-conversions, swagger
, text, tinylog, types-common, unix, wai, wai-predicates
, wai-routing, warp, warp-tls
}:
mkDerivation {
  pname = "wai-utilities";
  version = "0.16.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base bytestring bytestring-conversion errors exceptions
    http-types imports kan-extensions metrics-core metrics-wai pipes
    prometheus-client streaming-commons string-conversions swagger text
    tinylog types-common unix wai wai-predicates wai-routing warp
    warp-tls
  ];
  description = "Various helpers for WAI";
  license = lib.licenses.agpl3Only;
}
