{ mkDerivation, base, cassandra-util, conduit, imports, lens, lib
, optparse-applicative, tinylog, types-common, unliftio, wire-api
}:
mkDerivation {
  pname = "service-backfill";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base cassandra-util conduit imports lens optparse-applicative
    tinylog types-common unliftio wire-api
  ];
  description = "Backfill service tables";
  license = lib.licenses.agpl3Only;
  mainProgram = "service-backfill";
}
