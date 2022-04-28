{ mkDerivation, aeson, base, brig-types, bytestring, cassandra-util
, conduit, containers, filepath, galley, imports, iproute, lens
, lib, megaparsec, optparse-applicative, process, raw-strings-qq
, stache, text, time, tinylog, types-common, uuid, vector, wire-api
}:
mkDerivation {
  pname = "move-team";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base brig-types bytestring cassandra-util conduit containers
    filepath galley imports iproute lens megaparsec
    optparse-applicative process raw-strings-qq stache text time
    tinylog types-common uuid vector wire-api
  ];
  executableHaskellDepends = [
    aeson base brig-types bytestring cassandra-util conduit containers
    filepath galley imports iproute lens megaparsec
    optparse-applicative process raw-strings-qq stache text time
    tinylog types-common uuid vector wire-api
  ];
  description = "Export a team from one backend, or import it into another";
  license = lib.licenses.agpl3Only;
}
