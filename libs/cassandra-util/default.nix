{ mkDerivation, aeson, base, conduit, containers, cql, cql-io
, cql-io-tinylog, exceptions, imports, lens, lens-aeson, lib
, optparse-applicative, retry, split, text, time, tinylog, uuid
, wreq
}:
mkDerivation {
  pname = "cassandra-util";
  version = "0.16.5";
  src = ./.;
  libraryHaskellDepends = [
    aeson base conduit containers cql cql-io cql-io-tinylog exceptions
    imports lens lens-aeson optparse-applicative retry split text time
    tinylog uuid wreq
  ];
  description = "Cassandra Utilities";
  license = lib.licenses.agpl3Only;
}
