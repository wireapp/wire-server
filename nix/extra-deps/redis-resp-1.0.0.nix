{ mkDerivation, attoparsec, base, bytestring, bytestring-conversion
, containers, dlist, double-conversion, lib, operational
, semigroups, split, transformers
}:
mkDerivation {
  pname = "redis-resp";
  version = "1.0.0";
  sha256 = "c58cd29e345b2a9dba8c9dbed3bc8f01aed9d7e7e681c5b9893d80e4e407808b";
  libraryHaskellDepends = [
    attoparsec base bytestring bytestring-conversion containers dlist
    double-conversion operational semigroups split transformers
  ];
  homepage = "https://gitlab.com/twittner/redis-resp/";
  description = "REdis Serialization Protocol (RESP) implementation";
  license = lib.licenses.mpl20;
}
