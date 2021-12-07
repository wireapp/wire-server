{ mkDerivation, base, basement, bytestring, HUnit, lib, memory
, QuickCheck, test-framework, test-framework-hunit
, test-framework-quickcheck2
}:
mkDerivation {
  pname = "pem";
  version = "0.2.4";
  sha256 = "770c4c1b9cd24b3db7f511f8a48404a0d098999e28573c3743a8a296bb96f8d4";
  libraryHaskellDepends = [ base basement bytestring memory ];
  testHaskellDepends = [
    base bytestring HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2
  ];
  homepage = "http://github.com/vincenthz/hs-pem";
  description = "Privacy Enhanced Mail (PEM) format reader and writer";
  license = lib.licenses.bsd3;
}
