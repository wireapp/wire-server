# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, amazonka
, amazonka-core
, amazonka-ses
, async
, attoparsec
, base
, base16-bytestring
, bilge
, bloodhound
, bytestring
, bytestring-conversion
, case-insensitive
, cassandra-util
, conduit
, containers
, cql
, crypton
, currency-codes
, data-default
, data-timeout
, dns
, email-validate
, errors
, exceptions
, extended
, extra
, gitignoreSource
, HaskellNet
, HaskellNet-SSL
, HsOpenSSL
, hspec
, hspec-discover
, html-entities
, http-client
, http-types
, http2-manager
, imports
, iso639
, lens
, lib
, memory
, mime
, mime-mail
, network
, network-conduit-tls
, openapi3
, pipes
, polysemy
, polysemy-plugin
, polysemy-time
, polysemy-wire-zoo
, postie
, prometheus-client
, QuickCheck
, quickcheck-instances
, random
, resource-pool
, resourcet
, retry
, saml2-web-sso
, schema-profunctor
, scientific
, servant
, servant-client-core
, stomp-queue
, streaming-commons
, string-conversions
, template
, text
, text-icu-translit
, time
, time-out
, time-units
, tinylog
, transformers
, types-common
, unliftio
, unordered-containers
, uri-bytestring
, uuid
, wai-utilities
, wire-api
, wire-api-federation
, wire-otel
, witherable
, yaml
}:
mkDerivation {
  pname = "wire-subsystems";
  version = "0.1.0";
  src = gitignoreSource ./.;
  libraryHaskellDepends = [
    aeson
    amazonka
    amazonka-core
    amazonka-ses
    async
    attoparsec
    base
    base16-bytestring
    bilge
    bloodhound
    bytestring
    bytestring-conversion
    case-insensitive
    cassandra-util
    conduit
    containers
    cql
    crypton
    currency-codes
    data-default
    data-timeout
    dns
    errors
    exceptions
    extended
    extra
    HaskellNet
    HaskellNet-SSL
    HsOpenSSL
    hspec
    html-entities
    http-client
    http-types
    http2-manager
    imports
    iso639
    lens
    memory
    mime
    mime-mail
    network
    network-conduit-tls
    openapi3
    polysemy
    polysemy-plugin
    polysemy-time
    polysemy-wire-zoo
    prometheus-client
    QuickCheck
    resource-pool
    resourcet
    retry
    saml2-web-sso
    schema-profunctor
    servant
    servant-client-core
    stomp-queue
    template
    text
    text-icu-translit
    time
    time-out
    time-units
    tinylog
    transformers
    types-common
    unliftio
    unordered-containers
    uri-bytestring
    uuid
    wai-utilities
    wire-api
    wire-api-federation
    wire-otel
    witherable
  ];
  testHaskellDepends = [
    aeson
    async
    attoparsec
    base
    bilge
    bloodhound
    bytestring
    cassandra-util
    containers
    crypton
    data-default
    dns
    email-validate
    errors
    extended
    hspec
    imports
    iso639
    lens
    mime-mail
    network
    pipes
    polysemy
    polysemy-plugin
    polysemy-time
    polysemy-wire-zoo
    postie
    QuickCheck
    quickcheck-instances
    random
    scientific
    servant-client-core
    streaming-commons
    string-conversions
    text
    time
    tinylog
    transformers
    types-common
    wire-api
    wire-api-federation
    yaml
  ];
  testToolDepends = [ hspec-discover ];
  license = lib.licenses.agpl3Only;
}
