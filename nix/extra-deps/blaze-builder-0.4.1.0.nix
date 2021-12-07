{ mkDerivation, base, bytestring, deepseq, HUnit, lib, QuickCheck
, test-framework, test-framework-hunit, test-framework-quickcheck2
, text, utf8-string
}:
mkDerivation {
  pname = "blaze-builder";
  version = "0.4.1.0";
  sha256 = "91fc8b966f3e9dc9461e1675c7566b881740f99abc906495491a3501630bc814";
  revision = "1";
  editedCabalFile = "1p66mh9z3aqgind755xzf39pvl2hwjnwrlwiwyj653yzb1gn6c9j";
  libraryHaskellDepends = [ base bytestring deepseq text ];
  testHaskellDepends = [
    base bytestring HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2 text utf8-string
  ];
  homepage = "http://github.com/lpsmith/blaze-builder";
  description = "Efficient buffered output";
  license = lib.licenses.bsd3;
}
