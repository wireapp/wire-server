{ mkDerivation, base, lib }:
mkDerivation {
  pname = "iso639";
  version = "0.1.0.3";
  sha256 = "124b8322fabaedb4de3dbc39880b36d0eab0e28d5775954aadb6630bc0da25e8";
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/HugoDaniel/iso639";
  description = "ISO-639-1 language codes";
  license = lib.licenses.bsd3;
}
