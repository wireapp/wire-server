# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, base
, byteable
, bytestring
, gitignoreSource
, HsOpenSSL
, http-client
, imports
, lib
, safe-exceptions
, time
}:
mkDerivation {
  pname = "ssl-util";
  version = "0.1.0";
  src = gitignoreSource ./.;
  libraryHaskellDepends = [
    base
    byteable
    bytestring
    HsOpenSSL
    http-client
    imports
    safe-exceptions
    time
  ];
  description = "SSL-related utilities";
  license = lib.licenses.agpl3Only;
}
