{ mkDerivation, base, bytestring, deepseq, directory, hspec
, hspec-discover, HUnit, lib, QuickCheck, temporary
}:
mkDerivation {
  pname = "network";
  version = "3.1.2.0";
  sha256 = "e02abd759a09d689eb0b802ff3c4c734e5ba23fe6b96873b613603479452eb1f";
  revision = "1";
  editedCabalFile = "079svy0nr035xhz4gd6cila0wvsjl23hi3hq5407m3qdmcf4rkis";
  libraryHaskellDepends = [ base bytestring deepseq directory ];
  testHaskellDepends = [
    base bytestring directory hspec HUnit QuickCheck temporary
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/haskell/network";
  description = "Low-level networking interface";
  license = lib.licenses.bsd3;
}
