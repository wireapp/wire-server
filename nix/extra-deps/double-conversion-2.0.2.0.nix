{ mkDerivation, base, bytestring, ghc-prim, HUnit, lib
, test-framework, test-framework-hunit, test-framework-quickcheck2
, text
}:
mkDerivation {
  pname = "double-conversion";
  version = "2.0.2.0";
  sha256 = "44cde172395401169e844d6791b6eb0ef2c2e55a08de8dda96551cfe029ba26b";
  libraryHaskellDepends = [ base bytestring ghc-prim text ];
  testHaskellDepends = [
    base bytestring HUnit test-framework test-framework-hunit
    test-framework-quickcheck2 text
  ];
  homepage = "https://github.com/bos/double-conversion";
  description = "Fast conversion between double precision floating point and text";
  license = lib.licenses.bsd3;
}
