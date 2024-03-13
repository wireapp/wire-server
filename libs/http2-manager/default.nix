# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, async
, base
, bytestring
, containers
, gitignoreSource
, HsOpenSSL
, hspec
, hspec-discover
, http-types
, http2
, lib
, network
, random
, stm
, streaming-commons
, utf8-string
, text
, time-manager
}:
mkDerivation {
  pname = "http2-manager";
  version = "0.0.1";
  src = gitignoreSource ./.;
  libraryHaskellDepends = [
    async
    base
    bytestring
    containers
    HsOpenSSL
    http2
    network
    stm
    streaming-commons
    utf8-string
    text
    time-manager
  ];
  testHaskellDepends = [
    async
    base
    bytestring
    containers
    HsOpenSSL
    hspec
    http-types
    http2
    network
    random
    stm
    streaming-commons
    time-manager
  ];
  testToolDepends = [ hspec-discover ];
  description = "Managed connection pool for HTTP2";
  license = lib.licenses.agpl3Only;
}
