{ mkDerivation, aeson, attoparsec, base, brig, brig-types
, bytestring, bytestring-conversion, cassandra-util, conduit
, containers, filepath, galley, imports, iproute, lens, lib
, megaparsec, optparse-applicative, process, raw-strings-qq, stache
, text, time, tinylog, types-common, uuid, vector, wire-api
}:
mkDerivation {
  pname = "assets";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base brig brig-types bytestring
    bytestring-conversion cassandra-util conduit containers filepath
    galley imports iproute lens megaparsec optparse-applicative process
    raw-strings-qq stache text time tinylog types-common uuid vector
    wire-api
  ];
  executableHaskellDepends = [ base ];
  description = "Scan the brig user table, search for malformatted asset keys and print them";
  license = lib.licenses.agpl3Only;
  mainProgram = "assets";
}
