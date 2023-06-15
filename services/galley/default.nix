# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, aeson-qq
, amazonka
, amazonka-sqs
, amqp
, asn1-encoding
, asn1-types
, async
, base
, base64-bytestring
, bilge
, binary
, brig-types
, bytestring
, bytestring-conversion
, call-stack
, case-insensitive
, cassandra-util
, cassava
, cereal
, comonad
, conduit
, containers
, cookie
, cryptonite
, currency-codes
, data-default
, data-timeout
, directory
, either
, enclosed-exceptions
, errors
, exceptions
, extended
, extra
, federator
, filepath
, galley-types
, gitignoreSource
, gundeck-types
, hex
, HsOpenSSL
, hspec
, http-client
, http-client-openssl
, http-client-tls
, http-media
, http-types
, http2-manager
, imports
, kan-extensions
, lens
, lens-aeson
, lib
, memory
, metrics-core
, metrics-wai
, mtl
, network
, network-uri
, optparse-applicative
, pem
, polysemy
, polysemy-wire-zoo
, process
, proto-lens
, protobuf
, QuickCheck
, quickcheck-instances
, random
, raw-strings-qq
, resourcet
, retry
, safe-exceptions
, saml2-web-sso
, schema-profunctor
, servant
, servant-client
, servant-client-core
, servant-server
, singletons
, sop-core
, split
, ssl-util
, stm
, streaming-commons
, tagged
, tasty
, tasty-cannon
, tasty-hunit
, tasty-quickcheck
, temporary
, text
, time
, tinylog
, tls
, transformers
, transitive-anns
, types-common
, types-common-aws
, types-common-journal
, unix
, unliftio
, unordered-containers
, uri-bytestring
, uuid
, uuid-types
, vector
, wai
, wai-extra
, wai-middleware-gunzip
, wai-predicates
, wai-routing
, wai-utilities
, warp
, warp-tls
, wire-api
, wire-api-federation
, x509
, yaml
}:
mkDerivation {
  pname = "galley";
  version = "0.83.0";
  src = gitignoreSource ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    amazonka
    amazonka-sqs
    amqp
    asn1-encoding
    asn1-types
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
    cereal
    comonad
    containers
    cryptonite
    currency-codes
    data-default
    data-timeout
    either
    enclosed-exceptions
    errors
    exceptions
    extended
    extra
    galley-types
    gundeck-types
    hex
    HsOpenSSL
    http-client
    http-client-openssl
    http-media
    http-types
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
    proto-lens
    protobuf
    raw-strings-qq
    resourcet
    retry
    safe-exceptions
    saml2-web-sso
    schema-profunctor
    servant
    servant-client
    servant-server
    singletons
    split
    ssl-util
    stm
    tagged
    text
    time
    tinylog
    tls
    transformers
    transitive-anns
    types-common
    types-common-aws
    types-common-journal
    unliftio
    uri-bytestring
    uuid
    wai
    wai-extra
    wai-middleware-gunzip
    wai-predicates
    wai-routing
    wai-utilities
    wire-api
    wire-api-federation
    x509
  ];
  executableHaskellDepends = [
    aeson
    aeson-qq
    async
    base
    base64-bytestring
    bilge
    binary
    brig-types
    bytestring
    bytestring-conversion
    call-stack
    case-insensitive
    cassandra-util
    cassava
    cereal
    conduit
    containers
    cookie
    cryptonite
    currency-codes
    data-default
    data-timeout
    directory
    errors
    exceptions
    extended
    extra
    federator
    filepath
    galley-types
    hex
    HsOpenSSL
    hspec
    http-client
    http-client-openssl
    http-client-tls
    http-media
    http-types
    imports
    kan-extensions
    lens
    lens-aeson
    memory
    metrics-wai
    mtl
    network
    network-uri
    optparse-applicative
    pem
    process
    proto-lens
    protobuf
    QuickCheck
    quickcheck-instances
    random
    raw-strings-qq
    retry
    saml2-web-sso
    schema-profunctor
    servant-client
    servant-client-core
    servant-server
    singletons
    sop-core
    ssl-util
    streaming-commons
    tagged
    tasty
    tasty-cannon
    tasty-hunit
    temporary
    text
    time
    tinylog
    transformers
    types-common
    types-common-aws
    types-common-journal
    unix
    unliftio
    unordered-containers
    uuid
    vector
    wai
    wai-extra
    wai-utilities
    warp
    warp-tls
    wire-api
    wire-api-federation
    yaml
  ];
  testHaskellDepends = [
    base
    containers
    extra
    galley-types
    imports
    lens
    polysemy
    polysemy-wire-zoo
    QuickCheck
    tasty
    tasty-hunit
    tasty-quickcheck
    types-common
    uuid-types
    wire-api
    wire-api-federation
  ];
  description = "Conversations";
  license = lib.licenses.agpl3Only;
}
