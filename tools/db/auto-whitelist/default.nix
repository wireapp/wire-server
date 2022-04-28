{ mkDerivation, base, cassandra-util, extra, imports, lens, lib
, optparse-applicative, tinylog, types-common, unliftio, wire-api
}:
mkDerivation {
  pname = "auto-whitelist";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base cassandra-util extra imports lens optparse-applicative tinylog
    types-common unliftio wire-api
  ];
  description = "Backfill service tables";
  license = lib.licenses.agpl3Only;
  mainProgram = "auto-whitelist";
}
