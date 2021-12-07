{ mkDerivation, base, bytestring, criterion, hspec, lib }:
mkDerivation {
  pname = "word8";
  version = "0.1.3";
  sha256 = "2630934c75728bfbf390c1f0206b225507b354f68d4047b06c018a36823b5d8a";
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  benchmarkHaskellDepends = [ base bytestring criterion ];
  description = "Word8 library";
  license = lib.licenses.bsd3;
}
