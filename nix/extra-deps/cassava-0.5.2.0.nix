{ mkDerivation, array, attoparsec, base, bytestring, containers
, deepseq, hashable, HUnit, lib, Only, QuickCheck
, quickcheck-instances, scientific, test-framework
, test-framework-hunit, test-framework-quickcheck2, text
, text-short, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "cassava";
  version = "0.5.2.0";
  sha256 = "b30d2ad5894519e364130c0510f167a4ffaf0e08a1e24c9a64238c855bfe0106";
  revision = "6";
  editedCabalFile = "0r3daq24ng1sbypxr1h8m33pibwvrxyzd2inwhi5pvjckn2ghpmq";
  configureFlags = [ "-f-bytestring--lt-0_10_4" ];
  libraryHaskellDepends = [
    array attoparsec base bytestring containers deepseq hashable Only
    scientific text text-short transformers unordered-containers vector
  ];
  testHaskellDepends = [
    attoparsec base bytestring hashable HUnit QuickCheck
    quickcheck-instances scientific test-framework test-framework-hunit
    test-framework-quickcheck2 text unordered-containers vector
  ];
  homepage = "https://github.com/hvr/cassava";
  description = "A CSV parsing and encoding library";
  license = lib.licenses.bsd3;
}
