{ mkDerivation, base, bytestring, deepseq, hspec, lib, QuickCheck
, quickcheck-instances, text, utf8-string
}:
mkDerivation {
  pname = "string-conversions";
  version = "0.4.0.1";
  sha256 = "46bcce6d9ce62c558b7658a75d9c6a62f7259d6b0473d011d8078234ad6a1994";
  libraryHaskellDepends = [ base bytestring text utf8-string ];
  testHaskellDepends = [
    base bytestring deepseq hspec QuickCheck quickcheck-instances text
    utf8-string
  ];
  homepage = "https://github.com/soenkehahn/string-conversions#readme";
  description = "Simplifies dealing with different types for strings";
  license = lib.licenses.bsd3;
}
