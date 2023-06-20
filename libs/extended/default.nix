# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, amqp
, base
, bytestring
, cassandra-util
, containers
, errors
, exceptions
, extra
, first-class-families
, gitignoreSource
, HsOpenSSL
, hspec
, hspec-discover
, http-client
, http-client-openssl
, http-types
, imports
, lib
, metrics-wai
, monad-control
, optparse-applicative
, resourcet
, retry
, servant
, servant-client
, servant-client-core
, servant-queryparam-client
, servant-queryparam-core
, servant-server
, servant-swagger
, temporary
, text
, tinylog
, unliftio
, wai
}:
mkDerivation {
  pname = "extended";
  version = "0.1.0";
  src = gitignoreSource ./.;
  libraryHaskellDepends = [
    aeson
    amqp
    base
    bytestring
    cassandra-util
    containers
    errors
    exceptions
    extra
    first-class-families
    HsOpenSSL
    http-client
    http-client-openssl
    http-types
    imports
    metrics-wai
    monad-control
    optparse-applicative
    resourcet
    retry
    servant
    servant-client
    servant-client-core
    servant-queryparam-client
    servant-queryparam-core
    servant-server
    servant-swagger
    text
    tinylog
    unliftio
    wai
  ];
  testHaskellDepends = [ aeson base hspec imports temporary ];
  testToolDepends = [ hspec-discover ];
  description = "Extended versions of common modules";
  license = lib.licenses.agpl3Only;
}
