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
, data-default
, data-timeout
, enclosed-exceptions
, errors
, exceptions
, extended
, extra
, federator
, filepath
, galley-types
, gitignoreSource
, hasql-pool
, hex
, hs-opentelemetry-instrumentation-wai
, hs-opentelemetry-sdk
, HsOpenSSL
, http-api-data
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
, prometheus-client
, proto-lens
, protobuf
, QuickCheck
, quickcheck-instances
, random
, raw-strings-qq
, resourcet
, retry
, safe-exceptions
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
, string-conversions
, tagged
, tasty
, tasty-ant-xml
, tasty-cannon
, tasty-hunit
, tasty-quickcheck
, temporary
, text
, time
, tinylog
, tls
, transformers
, types-common
, types-common-aws
, types-common-journal
, unix
, unliftio
, unordered-containers
, uri-bytestring
, utf8-string
, uuid
, uuid-types
, wai
, wai-extra
, wai-middleware-gunzip
, wai-utilities
, warp
, warp-tls
, wire-api
, wire-api-federation
, wire-otel
, wire-subsystems
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
    cassandra-util
    cassava
    comonad
    constraints
    containers
    crypton
    crypton-x509
    currency-codes
    data-default
    data-timeout
    enclosed-exceptions
    errors
    exceptions
    extended
    extra
    galley-types
    hasql-pool
    hex
    hs-opentelemetry-instrumentation-wai
    hs-opentelemetry-sdk
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
    optparse-applicative
    pem
    polysemy
    polysemy-wire-zoo
    prometheus-client
    proto-lens
    raw-strings-qq
    resourcet
    retry
    safe-exceptions
    servant
    servant-client
    servant-client-core
    servant-server
    singletons
    sop-core
    split
    ssl-util
    stm
    tagged
    text
    time
    tinylog
    tls
    transformers
    types-common
    types-common-aws
    types-common-journal
    unliftio
    uri-bytestring
    utf8-string
    uuid
    wai
    wai-extra
    wai-middleware-gunzip
    wai-utilities
    wire-api
    wire-api-federation
    wire-otel
    wire-subsystems
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
    cassandra-util
    cereal
    conduit
    containers
    cookie
    currency-codes
    data-default
    data-timeout
    errors
    exceptions
    extended
    extra
    federator
    filepath
    galley-types
    HsOpenSSL
    http-api-data
    http-client
    http-client-openssl
    http-client-tls
    http-types
    imports
    kan-extensions
    lens
    lens-aeson
    memory
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
    retry
    servant-client
    servant-client-core
    servant-server
    singletons
    sop-core
    ssl-util
    streaming-commons
    string-conversions
    tagged
    tasty
    tasty-ant-xml
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
    wai
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
