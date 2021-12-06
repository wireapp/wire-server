{ mkDerivation, aeson, base, bloodhound, brig-types, cassandra-util
, conduit, containers, hpack, http-client, imports, lens, lib
, optparse-applicative, text, tinylog, uuid
}:
mkDerivation {
  pname = "find-undead";
  version = "1.0.0";
  src = /home/axeman/workspace/wire-server/tools/db/find-undead;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bloodhound brig-types cassandra-util conduit containers
    http-client imports lens optparse-applicative text tinylog uuid
  ];
  prePatch = "hpack";
  description = "Backfill billing_team_member table";
  license = lib.licenses.agpl3Only;
}
