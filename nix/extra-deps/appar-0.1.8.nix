{ mkDerivation, base, bytestring, lib }:
mkDerivation {
  pname = "appar";
  version = "0.1.8";
  sha256 = "c4ceeddc26525b58d82c41b6d3e32141371a200a6794aae185b6266ccc81631f";
  libraryHaskellDepends = [ base bytestring ];
  description = "A simple applicative parser";
  license = lib.licenses.bsd3;
}
