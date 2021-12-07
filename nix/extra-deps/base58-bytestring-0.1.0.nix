{ mkDerivation, base, bytestring, criterion, lib
, quickcheck-assertions, quickcheck-instances, tasty
, tasty-quickcheck
}:
mkDerivation {
  pname = "base58-bytestring";
  version = "0.1.0";
  sha256 = "c2dbf598f3415053e12cca84b90fa7c0c1b02f3b784cce0157264baebf2d40d3";
  libraryHaskellDepends = [ base bytestring ];
  testHaskellDepends = [
    base bytestring quickcheck-assertions quickcheck-instances tasty
    tasty-quickcheck
  ];
  benchmarkHaskellDepends = [ base bytestring criterion ];
  homepage = "https://bitbucket.org/s9gf4ult/base58-bytestring";
  description = "Implementation of BASE58 transcoding for ByteStrings";
  license = lib.licenses.publicDomain;
}
