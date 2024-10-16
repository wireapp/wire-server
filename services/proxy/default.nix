# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, base
, bilge
, bytestring
, case-insensitive
, configurator
, errors
, exceptions
, extended
, gitignoreSource
, http-client
, http-client-tls
, http-reverse-proxy
, http-types
, imports
, lens
, lib
, metrics-wai
, retry
, servant-server
, text
, tinylog
, types-common
, unliftio-core
, uuid
, wai
, wai-middleware-gunzip
, wai-predicates
, wai-routing
, wai-utilities
, wire-api
}:
mkDerivation {
  pname = "proxy";
  version = "0.9.0";
  src = gitignoreSource ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    base
    bilge
    bytestring
    case-insensitive
    configurator
    errors
    exceptions
    extended
    http-client
    http-client-tls
    http-reverse-proxy
    http-types
    imports
    lens
    metrics-wai
    retry
    servant-server
    text
    tinylog
    types-common
    unliftio-core
    uuid
    wai
    wai-middleware-gunzip
    wai-predicates
    wai-routing
    wai-utilities
    wire-api
  ];
  executableHaskellDepends = [ base imports types-common ];
  license = lib.licenses.agpl3Only;
  mainProgram = "proxy";
}
