# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, base
, brig
, brig-types
, case-insensitive
, cassandra-util
, conduit
, containers
, extended
, galley-types
, gitignoreSource
, http-client
, imports
, lens
, lib
, optparse-applicative
, saml2-web-sso
, string-conversions
, text
, time
, tinylog
, types-common
, uri-bytestring
, uuid
, wire-api
}:
mkDerivation {
  pname = "scim-emails";
  version = "1.0.0";
  src = gitignoreSource ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson
    base
    brig
    brig-types
    case-insensitive
    cassandra-util
    conduit
    containers
    extended
    galley-types
    http-client
    imports
    lens
    optparse-applicative
    saml2-web-sso
    string-conversions
    text
    time
    tinylog
    types-common
    uri-bytestring
    uuid
    wire-api
  ];
  description = "Find inconsistencies between scim's external-id, user's email and user_keys table";
  license = lib.licenses.agpl3Only;
  mainProgram = "scim-emails";
}
