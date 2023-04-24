# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, async
, base
, bilge
, binary
, bytestring
, bytestring-conversion
, connection
, containers
, cryptonite
, data-default
, dns
, dns-util
, exceptions
, extended
, filepath
, gitignoreSource
, hinotify
, HsOpenSSL
, hspec
, http-client
, http-client-tls
, http-media
, http-types
, http2
, http2-manager
, imports
, interpolate
, kan-extensions
, lens
, lib
, metrics-core
, metrics-wai
, mtl
, optparse-applicative
, pem
, polysemy
, polysemy-wire-zoo
, QuickCheck
, random
, servant
, servant-client
, servant-client-core
, string-conversions
, tasty
, tasty-hunit
, tasty-quickcheck
, temporary
, text
, tinylog
, transformers
, types-common
, unix
, uuid
, wai
, wai-extra
, wai-utilities
, warp
, warp-tls
, wire-api
, wire-api-federation
, x509
, x509-validation
, yaml
}:
mkDerivation {
  pname = "federator";
  version = "1.0.0";
  src = gitignoreSource ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    async
    base
    bilge
    binary
    bytestring
    bytestring-conversion
    containers
    data-default
    dns
    dns-util
    exceptions
    extended
    filepath
    hinotify
    HsOpenSSL
    http-client
    http-media
    http-types
    http2
    http2-manager
    imports
    kan-extensions
    lens
    metrics-core
    metrics-wai
    mtl
    pem
    polysemy
    polysemy-wire-zoo
    servant
    servant-client-core
    string-conversions
    text
    tinylog
    transformers
    types-common
    unix
    wai
    wai-utilities
    warp
    wire-api-federation
    x509
    x509-validation
  ];
  executableHaskellDepends = [
    aeson
    base
    bilge
    binary
    bytestring
    bytestring-conversion
    connection
    cryptonite
    dns-util
    exceptions
    HsOpenSSL
    hspec
    http-client-tls
    http-types
    http2-manager
    imports
    kan-extensions
    lens
    mtl
    optparse-applicative
    polysemy
    QuickCheck
    random
    servant-client-core
    string-conversions
    tasty-hunit
    text
    types-common
    uuid
    wai-utilities
    wire-api
    wire-api-federation
    yaml
  ];
  testHaskellDepends = [
    aeson
    base
    bytestring
    bytestring-conversion
    containers
    data-default
    dns-util
    filepath
    HsOpenSSL
    http-media
    http-types
    http2
    http2-manager
    imports
    interpolate
    kan-extensions
    mtl
    polysemy
    polysemy-wire-zoo
    QuickCheck
    servant
    servant-client
    servant-client-core
    string-conversions
    tasty
    tasty-hunit
    tasty-quickcheck
    temporary
    text
    transformers
    types-common
    unix
    wai
    wai-extra
    wai-utilities
    warp
    warp-tls
    wire-api
    wire-api-federation
    x509-validation
    yaml
  ];
  description = "Federation Service";
  license = lib.licenses.agpl3Only;
}
