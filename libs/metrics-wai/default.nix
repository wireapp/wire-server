{ mkDerivation, base, bytestring, containers, hspec, hspec-discover
, http-types, imports, lib, metrics-core, servant
, servant-multipart, string-conversions, text, wai
, wai-middleware-prometheus, wai-route, wai-routing
}:
mkDerivation {
  pname = "metrics-wai";
  version = "0.5.7";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers http-types imports metrics-core servant
    servant-multipart string-conversions text wai
    wai-middleware-prometheus wai-route wai-routing
  ];
  testHaskellDepends = [
    base bytestring containers hspec http-types imports metrics-core
    servant servant-multipart string-conversions text wai
    wai-middleware-prometheus wai-route wai-routing
  ];
  testToolDepends = [ hspec-discover ];
  description = "Metrics WAI integration";
  license = lib.licenses.agpl3Only;
}
