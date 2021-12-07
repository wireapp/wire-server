{ mkDerivation, aeson, base, bson, deepseq, hspec, lib, QuickCheck
, random, safe, text
}:
mkDerivation {
  pname = "currency-codes";
  version = "3.0.0.1";
  sha256 = "48c5bc7e4054c2eec32e036b4351c6ddc64a9ab1354b75b82c93a27697c77e2c";
  libraryHaskellDepends = [
    aeson base bson deepseq random safe text
  ];
  testHaskellDepends = [ aeson base bson hspec QuickCheck ];
  homepage = "https://github.com/chordify/currency-codes";
  description = "ISO-4217 Currency Codes";
  license = lib.licenses.mit;
}
