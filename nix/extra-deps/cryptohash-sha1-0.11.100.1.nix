{ mkDerivation, base, base16-bytestring, bytestring, criterion, lib
, SHA, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "cryptohash-sha1";
  version = "0.11.100.1";
  sha256 = "3c79af33542512442f8f87f6abb1faef7cd43bbfb2859260a33251d861eb0dab";
  revision = "6";
  editedCabalFile = "10rpxrmqgwihmplczglwxf5q3l13z9j3kvi065z884y4dymmnkgc";
  libraryHaskellDepends = [ base bytestring ];
  testHaskellDepends = [
    base base16-bytestring bytestring SHA tasty tasty-hunit
    tasty-quickcheck
  ];
  benchmarkHaskellDepends = [ base bytestring criterion ];
  homepage = "https://github.com/hvr/cryptohash-sha1";
  description = "Fast, pure and practical SHA-1 implementation";
  license = lib.licenses.bsd3;
}
