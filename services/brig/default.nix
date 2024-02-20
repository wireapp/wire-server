# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, amazonka
, amazonka-core
, amazonka-dynamodb
, amazonka-ses
, amazonka-sqs
, amqp
, async
, attoparsec
, auto-update
, base
, base-prelude
, base16-bytestring
, base64-bytestring
, bilge
, binary
, bloodhound
, brig-types
, bytestring
, bytestring-conversion
, case-insensitive
, cassandra-util
, comonad
, conduit
, containers
, cookie
, cql
, cryptobox-haskell
, crypton-connection
, currency-codes
, data-default
, data-timeout
, dns
, dns-util
, email-validate
, enclosed-exceptions
, errors
, exceptions
, extended
, extra
, federator
, file-embed
, file-embed-lzma
, filepath
, fsnotify
, galley-types
, gitignoreSource
, gundeck-types
, hashable
, HaskellNet
, HaskellNet-SSL
, hscim
, HsOpenSSL
, html-entities
, http-api-data
, http-client
, http-client-openssl
, http-client-tls
, http-media
, http-reverse-proxy
, http-types
, http2-manager
, imports
, insert-ordered-containers
, iproute
, iso639
, jose
, jwt-tools
, lens
, lens-aeson
, lib
, metrics-core
, metrics-wai
, mime
, mime-mail
, mmorph
, MonadRandom
, mtl
, mwc-random
, network
, network-conduit-tls
, network-uri
, openapi3
, optparse-applicative
, pem
, pipes
, polysemy
, polysemy-conc
, polysemy-plugin
, polysemy-time
, polysemy-wire-zoo
, postie
, process
, proto-lens
, QuickCheck
, random
, random-shuffle
, raw-strings-qq
, resource-pool
, resourcet
, retry
, ropes
, safe
, safe-exceptions
, saml2-web-sso
, schema-profunctor
, scientific
, servant
, servant-client
, servant-client-core
, servant-openapi3
, servant-server
, servant-swagger-ui
, sodium-crypto-sign
, spar
, split
, ssl-util
, statistics
, stomp-queue
, streaming-commons
, tasty
, tasty-ant-xml
, tasty-cannon
, tasty-hunit
, tasty-quickcheck
, template
, template-haskell
, temporary
, text
, text-icu-translit
, time
, time-out
, time-units
, tinylog
, transformers
, transitive-anns
, types-common
, types-common-aws
, types-common-journal
, unliftio
, unordered-containers
, uri-bytestring
, uuid
, vector
, wai
, wai-extra
, wai-middleware-gunzip
, wai-predicates
, wai-route
, wai-routing
, wai-utilities
, warp
, warp-tls
, wire-api
, wire-api-federation
, wire-subsystems
, yaml
, zauth
}:
mkDerivation {
  pname = "brig";
  version = "2.0";
  src = gitignoreSource ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    amazonka
    amazonka-core
    amazonka-dynamodb
    amazonka-ses
    amazonka-sqs
    amqp
    async
    auto-update
    base
    base-prelude
    base16-bytestring
    base64-bytestring
    bilge
    bloodhound
    brig-types
    bytestring
    bytestring-conversion
    cassandra-util
    comonad
    conduit
    containers
    cookie
    cql
    cryptobox-haskell
    currency-codes
    data-timeout
    dns
    dns-util
    enclosed-exceptions
    errors
    exceptions
    extended
    extra
    file-embed
    file-embed-lzma
    filepath
    fsnotify
    galley-types
    gundeck-types
    hashable
    HaskellNet
    HaskellNet-SSL
    HsOpenSSL
    html-entities
    http-client
    http-client-openssl
    http-media
    http-types
    http2-manager
    imports
    insert-ordered-containers
    iproute
    iso639
    jose
    jwt-tools
    lens
    lens-aeson
    metrics-core
    metrics-wai
    mime
    mime-mail
    mmorph
    MonadRandom
    mtl
    mwc-random
    network
    network-conduit-tls
    openapi3
    optparse-applicative
    polysemy
    polysemy-conc
    polysemy-plugin
    polysemy-time
    polysemy-wire-zoo
    proto-lens
    random-shuffle
    raw-strings-qq
    resource-pool
    resourcet
    retry
    ropes
    safe-exceptions
    saml2-web-sso
    schema-profunctor
    scientific
    servant
    servant-openapi3
    servant-server
    servant-swagger-ui
    sodium-crypto-sign
    split
    ssl-util
    statistics
    stomp-queue
    template
    template-haskell
    text
    text-icu-translit
    time
    time-out
    time-units
    tinylog
    transformers
    transitive-anns
    types-common
    types-common-aws
    types-common-journal
    unliftio
    unordered-containers
    uri-bytestring
    uuid
    vector
    wai
    wai-extra
    wai-middleware-gunzip
    wai-predicates
    wai-routing
    wai-utilities
    wire-api
    wire-api-federation
    wire-subsystems
    yaml
    zauth
  ];
  executableHaskellDepends = [
    aeson
    async
    attoparsec
    base
    base16-bytestring
    bilge
    bloodhound
    brig-types
    bytestring
    bytestring-conversion
    case-insensitive
    cassandra-util
    containers
    cookie
    data-default
    data-timeout
    email-validate
    exceptions
    extended
    extra
    federator
    filepath
    galley-types
    hscim
    HsOpenSSL
    http-api-data
    http-client
    http-client-tls
    http-media
    http-reverse-proxy
    http-types
    imports
    jose
    lens
    lens-aeson
    metrics-wai
    mime
    mime-mail
    MonadRandom
    mtl
    network
    network-uri
    optparse-applicative
    pem
    pipes
    polysemy
    polysemy-wire-zoo
    postie
    process
    proto-lens
    QuickCheck
    random
    random-shuffle
    raw-strings-qq
    retry
    safe
    saml2-web-sso
    servant
    servant-client
    servant-client-core
    spar
    streaming-commons
    tasty
    tasty-ant-xml
    tasty-cannon
    tasty-hunit
    temporary
    text
    time
    time-units
    tinylog
    transformers
    types-common
    types-common-aws
    types-common-journal
    unliftio
    unordered-containers
    uri-bytestring
    uuid
    vector
    wai
    wai-extra
    wai-route
    wai-utilities
    warp
    warp-tls
    wire-api
    wire-api-federation
    yaml
    zauth
  ];
  testHaskellDepends = [
    aeson
    base
    binary
    brig-types
    bytestring
    containers
    crypton-connection
    data-timeout
    dns
    dns-util
    exceptions
    HsOpenSSL
    http-client
    http-client-tls
    http-types
    imports
    lens
    network
    polysemy
    polysemy-wire-zoo
    ssl-util
    streaming-commons
    tasty
    tasty-hunit
    tasty-quickcheck
    time
    tinylog
    types-common
    unliftio
    uri-bytestring
    uuid
    wai
    wai-route
    warp
    warp-tls
    wire-api
  ];
  description = "User Service";
  license = lib.licenses.agpl3Only;
}
