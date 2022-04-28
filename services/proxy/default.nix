{ mkDerivation, aeson, base, bilge, bytestring, case-insensitive
, configurator, data-default, exceptions, extended, http-client
, http-client-tls, http-reverse-proxy, http-types, imports, lens
, lib, metrics-wai, retry, text, tinylog, types-common
, unliftio-core, wai, wai-predicates, wai-routing, wai-utilities
, warp, wire-api
}:
mkDerivation {
  pname = "proxy";
  version = "0.9.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bilge bytestring case-insensitive configurator
    data-default exceptions extended http-client http-client-tls
    http-reverse-proxy http-types imports lens metrics-wai retry text
    tinylog types-common unliftio-core wai wai-predicates wai-routing
    wai-utilities warp wire-api
  ];
  executableHaskellDepends = [ base extended imports types-common ];
  license = lib.licenses.agpl3Only;
  mainProgram = "proxy";
}
