{ mkDerivation, base, bytestring, lib, pretty, QuickCheck
, semigroups, test-framework, test-framework-quickcheck2, text
, text-latin1
}:
mkDerivation {
  pname = "text-printer";
  version = "0.5.0.1";
  sha256 = "58a7680fc75a058ef8a03a6d519d5266f204bae2eb30021663de135a1c31b518";
  libraryHaskellDepends = [
    base bytestring pretty semigroups text text-latin1
  ];
  testHaskellDepends = [
    base QuickCheck test-framework test-framework-quickcheck2
  ];
  homepage = "https://github.com/mvv/text-printer";
  description = "Abstract interface for text builders/printers";
  license = lib.licenses.bsd3;
}
