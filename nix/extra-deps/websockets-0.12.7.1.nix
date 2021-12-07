{ mkDerivation, async, attoparsec, base, base64-bytestring, binary
, bytestring, bytestring-builder, case-insensitive, clock
, containers, criterion, entropy, HUnit, lib, network, QuickCheck
, random, SHA, streaming-commons, test-framework
, test-framework-hunit, test-framework-quickcheck2, text
}:
mkDerivation {
  pname = "websockets";
  version = "0.12.7.1";
  sha256 = "d356076b1ee6afcaf6dc075c2fd4194ca58a5088f43ce2cd10803b17f8db14cb";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async attoparsec base base64-bytestring binary bytestring
    bytestring-builder case-insensitive clock containers entropy
    network random SHA streaming-commons text
  ];
  testHaskellDepends = [
    async attoparsec base base64-bytestring binary bytestring
    bytestring-builder case-insensitive clock containers entropy HUnit
    network QuickCheck random SHA streaming-commons test-framework
    test-framework-hunit test-framework-quickcheck2 text
  ];
  benchmarkHaskellDepends = [
    async attoparsec base base64-bytestring binary bytestring
    bytestring-builder case-insensitive clock containers criterion
    entropy network random SHA text
  ];
  doCheck = false;
  homepage = "http://jaspervdj.be/websockets";
  description = "A sensible and clean way to write WebSocket-capable servers in Haskell";
  license = lib.licenses.bsd3;
}
