{ mkDerivation, base, brig-types, cassandra-util, conduit, galley
, hpack, imports, lens, lib, optparse-applicative, tinylog
, types-common, unliftio, wire-api
}:
mkDerivation {
  pname = "migrate-sso-feature-flag";
  version = "1.0.0";
  src = /home/axeman/workspace/wire-server/tools/db/migrate-sso-feature-flag;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base brig-types cassandra-util conduit galley imports lens
    optparse-applicative tinylog types-common unliftio wire-api
  ];
  prePatch = "hpack";
  description = "Backfill sso feature flag into teams that already have an IdP";
  license = lib.licenses.agpl3Only;
}
