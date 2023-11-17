# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, aeson-diff
, aeson-pretty
, aeson-qq
, async
, attoparsec
, base
, base64-bytestring
, binary
, binary-parsers
, bytestring
, bytestring-arbitrary
, bytestring-conversion
, case-insensitive
, cassandra-util
, cassava
, cereal
, comonad
, conduit
, constraints
, containers
, cookie
, crypton
, crypton-x509
, currency-codes
, deriving-aeson
, deriving-swagger2
, either
, email-validate
, errors
, extended
, extra
, filepath
, generics-sop
, ghc-prim
, gitignoreSource
, hashable
, hex
, hostname-validate
, hscim
, HsOpenSSL
, hspec
, hspec-wai
, http-api-data
, http-client
, http-media
, http-types
, imports
, insert-ordered-containers
, iproute
, iso3166-country-codes
, iso639
, jose
, lens
, lib
, memory
, metrics-wai
, mime
, mtl
, openapi3
, pem
, polysemy
, process
, proto-lens
, protobuf
, QuickCheck
, quickcheck-instances
, random
, resourcet
, saml2-web-sso
, schema-profunctor
, scientific
, servant
, servant-client
, servant-client-core
, servant-conduit
, servant-multipart
, servant-openapi3
, servant-server
, singletons
, singletons-base
, singletons-th
, sop-core
, tagged
, tasty
, tasty-hspec
, tasty-hunit
, tasty-quickcheck
, text
, time
, transitive-anns
, types-common
, unliftio
, unordered-containers
, uri-bytestring
, utf8-string
, uuid
, vector
, wai
, wai-extra
, wai-utilities
, wai-websockets
, websockets
, wire-message-proto-lens
, zauth
}:
mkDerivation {
  pname = "wire-api";
  version = "0.1.0";
  src = gitignoreSource ./.;
  libraryHaskellDepends = [
    aeson
    async
    attoparsec
    base
    base64-bytestring
    binary
    binary-parsers
    bytestring
    bytestring-conversion
    case-insensitive
    cassandra-util
    cassava
    cereal
    comonad
    conduit
    constraints
    containers
    cookie
    crypton
    crypton-x509
    currency-codes
    deriving-aeson
    deriving-swagger2
    either
    email-validate
    errors
    extended
    extra
    filepath
    generics-sop
    ghc-prim
    hashable
    hostname-validate
    hscim
    HsOpenSSL
    http-api-data
    http-client
    http-media
    http-types
    imports
    insert-ordered-containers
    iproute
    iso3166-country-codes
    iso639
    jose
    lens
    memory
    metrics-wai
    mime
    mtl
    openapi3
    pem
    polysemy
    proto-lens
    protobuf
    QuickCheck
    quickcheck-instances
    random
    resourcet
    saml2-web-sso
    schema-profunctor
    scientific
    servant
    servant-client
    servant-client-core
    servant-conduit
    servant-multipart
    servant-openapi3
    servant-server
    singletons
    singletons-base
    singletons-th
    sop-core
    tagged
    text
    time
    transitive-anns
    types-common
    unordered-containers
    uri-bytestring
    utf8-string
    uuid
    vector
    wai
    wai-extra
    wai-utilities
    wai-websockets
    websockets
    wire-message-proto-lens
    zauth
  ];
  testHaskellDepends = [
    aeson
    aeson-diff
    aeson-pretty
    aeson-qq
    async
    base
    binary
    bytestring
    bytestring-arbitrary
    bytestring-conversion
    cassava
    containers
    crypton
    currency-codes
    either
    filepath
    hex
    hspec
    hspec-wai
    http-api-data
    http-types
    imports
    iso3166-country-codes
    iso639
    lens
    memory
    metrics-wai
    openapi3
    pem
    process
    proto-lens
    QuickCheck
    random
    saml2-web-sso
    schema-profunctor
    servant
    servant-server
    tasty
    tasty-hspec
    tasty-hunit
    tasty-quickcheck
    text
    time
    types-common
    unliftio
    uri-bytestring
    uuid
    vector
    wai
    wire-message-proto-lens
  ];
  license = lib.licenses.agpl3Only;
}
