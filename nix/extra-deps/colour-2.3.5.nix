{ mkDerivation, base, lib, QuickCheck, random, test-framework
, test-framework-quickcheck2
}:
mkDerivation {
  pname = "colour";
  version = "2.3.5";
  sha256 = "3b8d471979617dce7c193523743c9782df63433d8e87e3ef6d97922e0da104e7";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [
    base QuickCheck random test-framework test-framework-quickcheck2
  ];
  homepage = "http://www.haskell.org/haskellwiki/Colour";
  description = "A model for human colour/color perception";
  license = lib.licenses.mit;
}
