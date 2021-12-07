{ mkDerivation, base, bytestring, containers, criterion, deepseq
, HUnit, lib, QuickCheck, split, test-framework
, test-framework-hunit, test-framework-quickcheck2
}:
mkDerivation {
  pname = "base64-bytestring";
  version = "1.0.0.3";
  sha256 = "ef159d60ec14c0a3f3e26bab5c9fd7634d5e1b983c6a64f0b0c3261efe008fc7";
  revision = "1";
  editedCabalFile = "0nc2m0by303x7zd0cshpvlhyapkabfhgbcaflg7jgaryssk7w0nm";
  libraryHaskellDepends = [ base bytestring ];
  testHaskellDepends = [
    base bytestring containers HUnit QuickCheck split test-framework
    test-framework-hunit test-framework-quickcheck2
  ];
  benchmarkHaskellDepends = [
    base bytestring containers criterion deepseq
  ];
  homepage = "https://github.com/haskell/base64-bytestring";
  description = "Fast base64 encoding and decoding for ByteStrings";
  license = lib.licenses.bsd3;
}
