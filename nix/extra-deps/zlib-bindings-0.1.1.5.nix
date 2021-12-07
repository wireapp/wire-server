{ mkDerivation, base, bytestring, hspec, lib, QuickCheck, zlib }:
mkDerivation {
  pname = "zlib-bindings";
  version = "0.1.1.5";
  sha256 = "c83bb438f9b6c5fe860982731eb8ac7eff993e8b56cbc15ef5b471f229f79109";
  revision = "2";
  editedCabalFile = "0fq49694gqkab8m0vq4i879blswczwd66n7xh4r4gwiahf0ryvqc";
  libraryHaskellDepends = [ base bytestring zlib ];
  testHaskellDepends = [ base bytestring hspec QuickCheck zlib ];
  homepage = "http://github.com/snapframework/zlib-bindings";
  description = "Low-level bindings to the zlib package";
  license = lib.licenses.bsd3;
}
