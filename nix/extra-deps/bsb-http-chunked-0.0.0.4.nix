{ mkDerivation, attoparsec, base, blaze-builder, bytestring
, deepseq, doctest, gauge, hedgehog, lib, semigroups, tasty
, tasty-hedgehog, tasty-hunit
}:
mkDerivation {
  pname = "bsb-http-chunked";
  version = "0.0.0.4";
  sha256 = "148309e23eb8b261c1de374712372d62d8c8dc8ee504c392809c7ec33c0a0e7c";
  revision = "3";
  editedCabalFile = "15hg352id2f4x0dnvv47bdiz6gv5hp5a2mki9yzmhc7ajpk31mdd";
  libraryHaskellDepends = [ base bytestring ];
  testHaskellDepends = [
    attoparsec base blaze-builder bytestring doctest hedgehog tasty
    tasty-hedgehog tasty-hunit
  ];
  benchmarkHaskellDepends = [
    base blaze-builder bytestring deepseq gauge semigroups
  ];
  homepage = "http://github.com/sjakobi/bsb-http-chunked";
  description = "Chunked HTTP transfer encoding for bytestring builders";
  license = lib.licenses.bsd3;
}
