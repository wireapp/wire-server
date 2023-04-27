# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, base
, dns
, gitignoreSource
, hspec
, hspec-discover
, imports
, iproute
, lib
, polysemy
, random
}:
mkDerivation {
  pname = "dns-util";
  version = "0.1.0";
  src = gitignoreSource ./.;
  libraryHaskellDepends = [
    base
    dns
    imports
    iproute
    polysemy
    random
  ];
  testHaskellDepends = [ base dns hspec imports ];
  testToolDepends = [ hspec-discover ];
  description = "Library to deal with DNS SRV records";
  license = lib.licenses.agpl3Only;
}
