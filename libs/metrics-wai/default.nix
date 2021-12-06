{ mkDerivation, base, bytestring, containers, hpack, hspec
, hspec-discover, http-types, imports, lib, metrics-core, servant
, servant-multipart, string-conversions, text, wai
, wai-middleware-prometheus, wai-route, wai-routing
}:
mkDerivation {
  pname = "metrics-wai";
  version = "0.5.7";
  src = /home/axeman/workspace/wire-server/libs/metrics-wai;
  libraryHaskellDepends = [
    base bytestring containers http-types imports metrics-core servant
    servant-multipart string-conversions text wai
    wai-middleware-prometheus wai-route wai-routing
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base bytestring containers hspec http-types imports metrics-core
    servant servant-multipart string-conversions text wai
    wai-middleware-prometheus wai-route wai-routing
  ];
  testToolDepends = [ hspec-discover ];
  prePatch = "hpack";
  description = "Metrics WAI integration";
  license = lib.licenses.agpl3Only;
}
