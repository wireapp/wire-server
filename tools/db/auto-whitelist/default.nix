{ mkDerivation, base, brig-types, cassandra-util, extra, hpack
, imports, lens, lib, optparse-applicative, tinylog, types-common
, unliftio
}:
mkDerivation {
  pname = "auto-whitelist";
  version = "1.0.0";
  src = /home/axeman/workspace/wire-server/tools/db/auto-whitelist;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base brig-types cassandra-util extra imports lens
    optparse-applicative tinylog types-common unliftio
  ];
  prePatch = "hpack";
  description = "Backfill service tables";
  license = lib.licenses.agpl3Only;
}
