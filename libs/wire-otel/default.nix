# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, base
, bytestring
, gitignoreSource
, hs-opentelemetry-api
, hs-opentelemetry-instrumentation-http-client
, hs-opentelemetry-instrumentation-wai
, hs-opentelemetry-sdk
, http-client
, http-semantics
, http-types
, http2
, http2-manager
, kan-extensions
, lib
, text
, unliftio
}:
mkDerivation {
  pname = "wire-otel";
  version = "0.1.0.0";
  src = gitignoreSource ./.;
  libraryHaskellDepends = [
    base
    bytestring
    hs-opentelemetry-api
    hs-opentelemetry-instrumentation-http-client
    hs-opentelemetry-instrumentation-wai
    hs-opentelemetry-sdk
    http-client
    http-semantics
    http-types
    http2
    http2-manager
    kan-extensions
    text
    unliftio
  ];
  testHaskellDepends = [ base ];
  homepage = "https://wire.com/";
  license = lib.licenses.agpl3Only;
}
