# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, async
, base
, bytestring
, bytestring-conversion
, errors
, exceptions
, gitignoreSource
, hspec
, hspec-discover
, http-types
, http2
, imports
, kan-extensions
, lib
, metrics-core
, openapi3
, pipes
, prometheus-client
, schema-profunctor
, servant-server
, streaming-commons
, temporary
, text
, tinylog
, types-common
, unix
, uuid
, wai
, wai-predicates
, warp
, warp-tls
}:
mkDerivation {
  pname = "wai-utilities";
  version = "0.16.1";
  src = gitignoreSource ./.;
  libraryHaskellDepends = [
    aeson
    async
    base
    bytestring
    bytestring-conversion
    errors
    exceptions
    http-types
    http2
    imports
    kan-extensions
    metrics-core
    openapi3
    pipes
    prometheus-client
    schema-profunctor
    servant-server
    streaming-commons
    text
    tinylog
    types-common
    unix
    uuid
    wai
    wai-predicates
    warp
    warp-tls
  ];
  testHaskellDepends = [
    bytestring
    hspec
    http-types
    imports
    temporary
    tinylog
    wai
  ];
  testToolDepends = [ hspec-discover ];
  description = "Various helpers for WAI";
  license = lib.licenses.agpl3Only;
}
