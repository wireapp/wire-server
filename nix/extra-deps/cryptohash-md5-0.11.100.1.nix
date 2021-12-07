{ mkDerivation, base, base16-bytestring, bytestring, criterion, lib
, pureMD5, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "cryptohash-md5";
  version = "0.11.100.1";
  sha256 = "710bd48770fa3e9a3b05428c6dc77fb72c91956d334a1eb89ded11bb843e18f9";
  revision = "6";
  editedCabalFile = "191nvffcrlyvr5dq99bbdxxl2qx44bla9adkhklyknf7ipqdd4yj";
  libraryHaskellDepends = [ base bytestring ];
  testHaskellDepends = [
    base base16-bytestring bytestring pureMD5 tasty tasty-hunit
    tasty-quickcheck
  ];
  benchmarkHaskellDepends = [ base bytestring criterion ];
  homepage = "https://github.com/hvr/cryptohash-md5";
  description = "Fast, pure and practical MD5 implementation";
  license = lib.licenses.bsd3;
}
