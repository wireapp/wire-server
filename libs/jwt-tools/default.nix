# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, base
, bytestring
, bytestring-conversion
, gitignoreSource
, hspec
, http-types
, imports
, lib
, rusty_jwt_tools_ffi
, string-conversions
, transformers
, utf8-string
}:
mkDerivation {
  pname = "jwt-tools";
  version = "0.1.0";
  src = gitignoreSource ./.;
  libraryHaskellDepends = [
    base
    bytestring-conversion
    http-types
    imports
    transformers
    utf8-string
  ];
  librarySystemDepends = [ rusty_jwt_tools_ffi ];
  testHaskellDepends = [
    bytestring
    hspec
    imports
    string-conversions
    transformers
  ];
  description = "FFI to rusty-jwt-tools";
  license = lib.licenses.agpl3Only;
}
