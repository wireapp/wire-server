# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, base
, bytestring
, cassandra-util
, containers
, gitignoreSource
, HsOpenSSL
, hspec
, hspec-discover
, imports
, jose
, lib
, polysemy
, polysemy-check
, polysemy-plugin
, QuickCheck
, saml2-web-sso
, string-conversions
, time
, tinylog
, types-common
, unliftio
, uuid
, wire-api
}:
mkDerivation {
  pname = "polysemy-wire-zoo";
  version = "0.1.0";
  src = gitignoreSource ./.;
  libraryHaskellDepends = [
    aeson
    base
    bytestring
    cassandra-util
    HsOpenSSL
    hspec
    imports
    jose
    polysemy
    polysemy-check
    polysemy-plugin
    QuickCheck
    saml2-web-sso
    string-conversions
    time
    tinylog
    types-common
    unliftio
    uuid
    wire-api
  ];
  testHaskellDepends = [
    base
    containers
    hspec
    imports
    polysemy
    polysemy-plugin
    unliftio
  ];
  testToolDepends = [ hspec-discover ];
  description = "Polysemy interface for various libraries";
  license = lib.licenses.agpl3Only;
}
