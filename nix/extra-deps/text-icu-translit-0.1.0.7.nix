{ mkDerivation, base, icu, lib, QuickCheck, test-framework
, test-framework-quickcheck2, text, text-icu
}:
mkDerivation {
  pname = "text-icu-translit";
  version = "0.1.0.7";
  sha256 = "028026a5a73e3bf5373de8895f66409e2841b0353468b7a99d4162af9a9fd5e1";
  libraryHaskellDepends = [ base text ];
  librarySystemDepends = [ icu ];
  testHaskellDepends = [
    base QuickCheck test-framework test-framework-quickcheck2 text
    text-icu
  ];
  description = "ICU transliteration";
  license = lib.licenses.bsd3;
}
