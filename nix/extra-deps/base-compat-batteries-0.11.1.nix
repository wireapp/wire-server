{ mkDerivation, base, base-compat, hspec, hspec-discover, lib
, QuickCheck
}:
mkDerivation {
  pname = "base-compat-batteries";
  version = "0.11.1";
  sha256 = "caf66fed3c0a3b0437692cad18c93a7074e8524a86b8be1ce04d0a18cbf6aed8";
  libraryHaskellDepends = [ base base-compat ];
  testHaskellDepends = [ base hspec QuickCheck ];
  testToolDepends = [ hspec-discover ];
  description = "base-compat with extra batteries";
  license = lib.licenses.mit;
}
