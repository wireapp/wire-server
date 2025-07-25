# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, aeson-pretty
, amazonka
, amazonka-core
, amazonka-sns
, amazonka-sqs
, amqp
, async
, attoparsec
, auto-update
, base
, base16-bytestring
, bilge
, bytestring
, bytestring-conversion
, cassandra-util
, conduit
, containers
, criterion
, crypton-x509-store
, data-timeout
, errors
, exceptions
, extended
, extra
, foldl
, gitignoreSource
, hedis
, hs-opentelemetry-instrumentation-wai
, hs-opentelemetry-sdk
, HsOpenSSL
, http-client
, http-client-tls
, http-types
, imports
, kan-extensions
, lens
, lens-aeson
, lib
, metrics-core
, metrics-wai
, MonadRandom
, mtl
, multiset
, network
, network-uri
, optparse-applicative
, prometheus-client
, psqueues
, QuickCheck
, quickcheck-instances
, quickcheck-state-machine
, random
, raw-strings-qq
, resourcet
, retry
, safe
, safe-exceptions
, scientific
, servant
, servant-server
, string-conversions
, tagged
, tasty
, tasty-ant-xml
, tasty-hunit
, tasty-quickcheck
, text
, these
, time
, tinylog
, tls
, types-common
, types-common-aws
, unliftio
, unordered-containers
, uuid
, wai
, wai-extra
, wai-middleware-gunzip
, wai-utilities
, websockets
, wire-api
, wire-otel
, yaml
}:
mkDerivation {
  pname = "gundeck";
  version = "1.45.0";
  src = gitignoreSource ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    amazonka
    amazonka-core
    amazonka-sns
    amazonka-sqs
    amqp
    async
    attoparsec
    auto-update
    base
    bilge
    bytestring
    bytestring-conversion
    cassandra-util
    containers
    crypton-x509-store
    data-timeout
    errors
    exceptions
    extended
    extra
    foldl
    hedis
    hs-opentelemetry-instrumentation-wai
    hs-opentelemetry-sdk
    http-client
    http-client-tls
    http-types
    imports
    lens
    lens-aeson
    metrics-core
    metrics-wai
    mtl
    network-uri
    prometheus-client
    psqueues
    raw-strings-qq
    resourcet
    retry
    safe-exceptions
    servant
    servant-server
    text
    these
    time
    tinylog
    tls
    types-common
    types-common-aws
    unliftio
    unordered-containers
    uuid
    wai
    wai-extra
    wai-middleware-gunzip
    wai-utilities
    wire-api
    wire-otel
    yaml
  ];
  executableHaskellDepends = [
    aeson
    async
    base
    base16-bytestring
    bilge
    bytestring
    bytestring-conversion
    cassandra-util
    conduit
    containers
    exceptions
    extended
    HsOpenSSL
    http-client
    http-client-tls
    imports
    kan-extensions
    lens
    lens-aeson
    network
    network-uri
    optparse-applicative
    random
    retry
    safe
    tagged
    tasty
    tasty-ant-xml
    tasty-hunit
    text
    time
    tinylog
    types-common
    uuid
    wai-utilities
    websockets
    wire-api
    yaml
  ];
  testHaskellDepends = [
    aeson
    aeson-pretty
    amazonka
    amazonka-core
    amqp
    async
    base
    bytestring-conversion
    containers
    exceptions
    HsOpenSSL
    imports
    lens
    MonadRandom
    mtl
    multiset
    network-uri
    QuickCheck
    quickcheck-instances
    quickcheck-state-machine
    scientific
    string-conversions
    tasty
    tasty-hunit
    tasty-quickcheck
    text
    these
    tinylog
    types-common
    wire-api
  ];
  benchmarkHaskellDepends = [
    amazonka
    base
    criterion
    HsOpenSSL
    imports
    lens
    random
    text
    types-common
    uuid
    wire-api
  ];
  description = "Push Notification Hub";
  license = lib.licenses.agpl3Only;
}
