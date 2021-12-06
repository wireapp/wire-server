{ mkDerivation, aeson, base, conduit, containers, cql, cql-io
, cql-io-tinylog, exceptions, hpack, imports, lens, lens-aeson, lib
, optparse-applicative, retry, split, text, time, tinylog, uuid
, wreq
}:
mkDerivation {
  pname = "cassandra-util";
  version = "0.16.5";
  src = /home/axeman/workspace/wire-server/libs/cassandra-util;
  libraryHaskellDepends = [
    aeson base conduit containers cql cql-io cql-io-tinylog exceptions
    imports lens lens-aeson optparse-applicative retry split text time
    tinylog uuid wreq
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  description = "Cassandra Utilities";
  license = lib.licenses.agpl3Only;
}
