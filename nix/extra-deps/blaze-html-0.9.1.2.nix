{ mkDerivation, base, blaze-builder, blaze-markup, bytestring
, containers, HUnit, lib, QuickCheck, test-framework
, test-framework-hunit, test-framework-quickcheck2, text
}:
mkDerivation {
  pname = "blaze-html";
  version = "0.9.1.2";
  sha256 = "60503f42546c6c1b954014d188ea137e43d74dcffd2bf6157c113fd91a0c394c";
  revision = "1";
  editedCabalFile = "0wvlfb3rd9cm3p894p5rl9kggrsr5da3n8x9ydrbagx91yvkxns9";
  libraryHaskellDepends = [
    base blaze-builder blaze-markup bytestring text
  ];
  testHaskellDepends = [
    base blaze-builder blaze-markup bytestring containers HUnit
    QuickCheck test-framework test-framework-hunit
    test-framework-quickcheck2 text
  ];
  homepage = "http://jaspervdj.be/blaze";
  description = "A blazingly fast HTML combinator library for Haskell";
  license = lib.licenses.bsd3;
}
