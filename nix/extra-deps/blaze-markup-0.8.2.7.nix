{ mkDerivation, base, blaze-builder, bytestring, containers, HUnit
, lib, QuickCheck, tasty, tasty-hunit, tasty-quickcheck, text
}:
mkDerivation {
  pname = "blaze-markup";
  version = "0.8.2.7";
  sha256 = "8b6489ed422d98e4372f41be1f40f772b8d4ed925f3203b36be4078d918c8ee4";
  libraryHaskellDepends = [ base blaze-builder bytestring text ];
  testHaskellDepends = [
    base blaze-builder bytestring containers HUnit QuickCheck tasty
    tasty-hunit tasty-quickcheck text
  ];
  homepage = "http://jaspervdj.be/blaze";
  description = "A blazingly fast markup combinator library for Haskell";
  license = lib.licenses.bsd3;
}
