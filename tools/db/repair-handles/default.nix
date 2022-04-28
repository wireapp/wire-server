{ mkDerivation, base, brig, cassandra-util, conduit, containers
, imports, lens, lib, mtl, optparse-applicative, string-conversions
, text, tinylog, types-common, uuid
}:
mkDerivation {
  pname = "repair-handles";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base brig cassandra-util conduit containers imports lens mtl
    optparse-applicative string-conversions text tinylog types-common
    uuid
  ];
  description = "Repair inconsistencies between tables user and user_handle";
  license = lib.licenses.agpl3Only;
  mainProgram = "repair-handles";
}
