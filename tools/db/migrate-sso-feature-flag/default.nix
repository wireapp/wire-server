{ mkDerivation, base, brig-types, cassandra-util, conduit, galley
, imports, lens, lib, optparse-applicative, tinylog, types-common
, unliftio, wire-api
}:
mkDerivation {
  pname = "migrate-sso-feature-flag";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base brig-types cassandra-util conduit galley imports lens
    optparse-applicative tinylog types-common unliftio wire-api
  ];
  description = "Backfill sso feature flag into teams that already have an IdP";
  license = lib.licenses.agpl3Only;
  mainProgram = "migrate-sso-feature-flag";
}
