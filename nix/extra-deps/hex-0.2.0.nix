{ mkDerivation, base, bytestring, lib }:
mkDerivation {
  pname = "hex";
  version = "0.2.0";
  sha256 = "6a93281ff0b105eccd1ecc73d3fa87171f0ba9c167a97dea488d1481ca3186d5";
  libraryHaskellDepends = [ base bytestring ];
  homepage = "https://github.com/taruti/haskell-hex";
  description = "Convert strings into hexadecimal and back";
  license = lib.licenses.bsd3;
}
