{ mkDerivation, base, brig, cassandra-util, conduit, containers
, hpack, imports, lens, lib, mtl, optparse-applicative
, string-conversions, text, tinylog, types-common, uuid
}:
mkDerivation {
  pname = "repair-handles";
  version = "1.0.0";
  src = /home/axeman/workspace/wire-server/tools/db/repair-handles;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base brig cassandra-util conduit containers imports lens mtl
    optparse-applicative string-conversions text tinylog types-common
    uuid
  ];
  prePatch = "hpack";
  description = "Repair inconsistencies between tables user and user_handle";
  license = lib.licenses.agpl3Only;
}
