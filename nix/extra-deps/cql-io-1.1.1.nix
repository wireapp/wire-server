{ mkDerivation, async, auto-update, base, bytestring, containers
, cql, cryptonite, data-default-class, Decimal, exceptions
, hashable, HsOpenSSL, iproute, lens, lib, mtl, mwc-random, network
, primes, raw-strings-qq, retry, semigroups, stm, tasty
, tasty-hunit, text, time, transformers, unliftio-core
, unordered-containers, uuid, vector
}:
mkDerivation {
  pname = "cql-io";
  version = "1.1.1";
  sha256 = "97fd0d2487d42f5256f5985cd5e5f2c56a52a90417ef32865a01e2e9624ae1fd";
  libraryHaskellDepends = [
    async auto-update base bytestring containers cql cryptonite
    data-default-class exceptions hashable HsOpenSSL iproute lens mtl
    mwc-random network retry semigroups stm text time transformers
    unliftio-core unordered-containers uuid vector
  ];
  testHaskellDepends = [
    async base containers cql Decimal iproute mtl primes raw-strings-qq
    tasty tasty-hunit text time uuid
  ];
  doHaddock = false;
  homepage = "https://gitlab.com/twittner/cql-io/";
  description = "Cassandra CQL client";
  license = lib.licenses.mpl20;
}
