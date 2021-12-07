{ mkDerivation, base, bytestring, ghc-prim, lib }:
mkDerivation {
  pname = "base16-bytestring";
  version = "0.1.1.7";
  sha256 = "525689679d5cc80fa532c1d5cfeace0f62bbb54134fad514f1ba00d0e7fe69ba";
  revision = "3";
  editedCabalFile = "1lrxqhbjsml0q1ahpcx7p0xjy6bj1m6qzjwmv841r5r8jrm2a880";
  libraryHaskellDepends = [ base bytestring ghc-prim ];
  testHaskellDepends = [ base bytestring ];
  homepage = "http://github.com/haskell/base16-bytestring";
  description = "Fast base16 (hex) encoding and decoding for ByteStrings";
  license = lib.licenses.bsd3;
}
