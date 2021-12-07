{ mkDerivation, base, bytestring, cereal, containers, Decimal
, iproute, lib, network, QuickCheck, tasty, tasty-quickcheck
, template-haskell, text, time, transformers, uuid, vector
}:
mkDerivation {
  pname = "cql";
  version = "4.0.2";
  sha256 = "f42a3a385faadb10d61ca1622815f77c4218df289b305e3a539ebc0a9501c82c";
  libraryHaskellDepends = [
    base bytestring cereal containers Decimal iproute network
    template-haskell text time transformers uuid vector
  ];
  testHaskellDepends = [
    base bytestring cereal Decimal iproute network QuickCheck tasty
    tasty-quickcheck text time uuid
  ];
  homepage = "https://gitlab.com/twittner/cql/";
  description = "Cassandra CQL binary protocol";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
