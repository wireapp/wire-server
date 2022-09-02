# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation, aeson, base, bloodhound, brig-types, cassandra-util
, conduit, containers, gitignoreSource, http-client, imports, lens
, lib, optparse-applicative, text, tinylog, uuid
}:
mkDerivation {
  pname = "find-undead";
  version = "1.0.0";
  src = gitignoreSource ./.;
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
