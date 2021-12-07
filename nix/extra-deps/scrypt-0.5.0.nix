{ mkDerivation, base, base64-bytestring, bytestring, entropy, HUnit
, lib, QuickCheck, test-framework, test-framework-hunit
, test-framework-quickcheck2
}:
mkDerivation {
  pname = "scrypt";
  version = "0.5.0";
  sha256 = "3ec0a622393e2a4dbbce4c899602c848d924f8516688491b1162331b7093d9b2";
  libraryHaskellDepends = [
    base base64-bytestring bytestring entropy
  ];
  testHaskellDepends = [
    base bytestring HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2
  ];
  homepage = "http://github.com/informatikr/scrypt";
  description = "Stronger password hashing via sequential memory-hard functions";
  license = lib.licenses.bsd3;
}
