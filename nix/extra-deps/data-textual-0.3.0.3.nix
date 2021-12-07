{ mkDerivation, base, bytestring, lib, parsers, QuickCheck
, test-framework, test-framework-quickcheck2, text, text-latin1
, text-printer, type-hint
}:
mkDerivation {
  pname = "data-textual";
  version = "0.3.0.3";
  sha256 = "4b9ee8ccd03f24203dd9307bf9aa67180ff0f07b45c3a01e33d8185ff275ec9a";
  libraryHaskellDepends = [
    base bytestring parsers text text-latin1 text-printer
  ];
  testHaskellDepends = [
    base parsers QuickCheck test-framework test-framework-quickcheck2
    text-printer type-hint
  ];
  homepage = "https://github.com/mvv/data-textual";
  description = "Human-friendly textual representations";
  license = lib.licenses.bsd3;
}
