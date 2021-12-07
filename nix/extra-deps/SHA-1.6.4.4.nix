{ mkDerivation, array, base, binary, bytestring, lib, QuickCheck
, test-framework, test-framework-quickcheck2
}:
mkDerivation {
  pname = "SHA";
  version = "1.6.4.4";
  sha256 = "6bd950df6b11a3998bb1452d875d2da043ee43385459afc5f16d471d25178b44";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ array base binary bytestring ];
  testHaskellDepends = [
    array base binary bytestring QuickCheck test-framework
    test-framework-quickcheck2
  ];
  description = "Implementations of the SHA suite of message digest functions";
  license = lib.licenses.bsd3;
}
