# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, base
, gitignoreSource
, hs-opentelemetry-instrumentation-http-client
, hs-opentelemetry-instrumentation-wai
, hs-opentelemetry-sdk
, http-client
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
    hs-opentelemetry-instrumentation-http-client
    hs-opentelemetry-instrumentation-wai
    hs-opentelemetry-sdk
    http-client
    kan-extensions
    text
    unliftio
  ];
  testHaskellDepends = [ base ];
  homepage = "https://wire.com/";
  license = lib.licenses.agpl3Only;
}
