{ mkDerivation, aeson, base, bloodhound, brig-types, cassandra-util
, conduit, containers, http-client, imports, lens, lib
, optparse-applicative, text, tinylog, uuid
}:
mkDerivation {
  pname = "find-undead";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bloodhound brig-types cassandra-util conduit containers
    http-client imports lens optparse-applicative text tinylog uuid
  ];
  description = "Backfill billing_team_member table";
  license = lib.licenses.agpl3Only;
  mainProgram = "find-undead";
}
