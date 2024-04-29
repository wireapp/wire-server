# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, aeson-qq
, async
, base
, base64-bytestring
, bilge
, brig-types
, bytestring
, bytestring-conversion
, case-insensitive
, cassandra-util
, cassava
, conduit
, containers
, cookie
, crypton
, crypton-x509
, email-validate
, exceptions
, extended
, gitignoreSource
, hscim
, HsOpenSSL
, hspec
, hspec-core
, hspec-discover
, hspec-junit-formatter
, hspec-wai
, http-api-data
, http-client
, http-types
, imports
, iso639
, lens
, lens-aeson
, lib
, metrics-wai
, MonadRandom
, mtl
, network-uri
, openapi3
, optparse-applicative
, polysemy
, polysemy-check
, polysemy-plugin
, polysemy-wire-zoo
, QuickCheck
, random
, raw-strings-qq
, retry
, saml2-web-sso
, servant
, servant-multipart
, servant-openapi3
, servant-server
, silently
, string-conversions
, tasty-hunit
, text
, text-latin1
, time
, tinylog
, transformers
, types-common
, uri-bytestring
, utf8-string
, uuid
, vector
, wai
, wai-extra
, wai-middleware-gunzip
, wai-utilities
, warp
, wire-api
, xml-conduit
, yaml
, zauth
}:
mkDerivation {
  pname = "spar";
  version = "0.1";
  src = gitignoreSource ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    base
    base64-bytestring
    bilge
    brig-types
    bytestring
    bytestring-conversion
    case-insensitive
    cassandra-util
    containers
    cookie
    crypton
    crypton-x509
    exceptions
    extended
    hscim
    hspec
    http-types
    imports
    lens
    metrics-wai
    mtl
    network-uri
    optparse-applicative
    polysemy
    polysemy-check
    polysemy-plugin
    polysemy-wire-zoo
    QuickCheck
    raw-strings-qq
    saml2-web-sso
    servant-multipart
    servant-server
    text
    text-latin1
    time
    tinylog
    transformers
    types-common
    uri-bytestring
    utf8-string
    uuid
    wai
    wai-middleware-gunzip
    wai-utilities
    warp
    wire-api
    yaml
  ];
  executableHaskellDepends = [
    aeson
    aeson-qq
    async
    base
    base64-bytestring
    bilge
    brig-types
    bytestring
    bytestring-conversion
    case-insensitive
    cassandra-util
    cassava
    conduit
    containers
    cookie
    crypton
    email-validate
    exceptions
    extended
    hscim
    HsOpenSSL
    hspec
    hspec-core
    hspec-junit-formatter
    hspec-wai
    http-api-data
    http-client
    http-types
    imports
    iso639
    lens
    lens-aeson
    MonadRandom
    mtl
    optparse-applicative
    polysemy
    polysemy-plugin
    polysemy-wire-zoo
    QuickCheck
    random
    raw-strings-qq
    retry
    saml2-web-sso
    servant
    servant-server
    silently
    string-conversions
    tasty-hunit
    text
    time
    tinylog
    transformers
    types-common
    uri-bytestring
    utf8-string
    uuid
    vector
    wai-extra
    wai-utilities
    warp
    wire-api
    xml-conduit
    yaml
    zauth
  ];
  executableToolDepends = [ hspec-discover ];
  testHaskellDepends = [
    aeson
    aeson-qq
    base
    brig-types
    bytestring-conversion
    cookie
    hscim
    hspec
    imports
    lens
    lens-aeson
    metrics-wai
    mtl
    network-uri
    openapi3
    polysemy
    polysemy-plugin
    polysemy-wire-zoo
    QuickCheck
    saml2-web-sso
    servant
    servant-openapi3
    string-conversions
    time
    tinylog
    types-common
    uri-bytestring
    uuid
    wire-api
  ];
  testToolDepends = [ hspec-discover ];
  description = "User Service for SSO (Single Sign-On) provisioning and authentication";
  license = lib.licenses.agpl3Only;
}
