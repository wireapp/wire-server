{ mkDerivation, base, cassandra-util, conduit, containers
, galley-types, hpack, imports, lens, lib, optparse-applicative
, text, tinylog, types-common
}:
mkDerivation {
  pname = "billing-team-member-backfill";
  version = "1.0.0";
  src = /home/axeman/workspace/wire-server/tools/db/billing-team-member-backfill;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base cassandra-util conduit containers galley-types imports lens
    optparse-applicative text tinylog types-common
  ];
  prePatch = "hpack";
  description = "Backfill billing_team_member table";
  license = lib.licenses.agpl3Only;
}
