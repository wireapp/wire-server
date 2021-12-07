{ mkDerivation, base, dec, lib }:
mkDerivation {
  pname = "singleton-bool";
  version = "0.1.5";
  sha256 = "405dd57dea92857c04f539c3394894c40c8103ea0c4f3f0fdbfbd8acccde899f";
  revision = "3";
  editedCabalFile = "11rhzpy4xiry39bbxzwrqff75f0f4g7z0vkr3v9l8rv3w40jlf7x";
  libraryHaskellDepends = [ base dec ];
  homepage = "https://github.com/phadej/singleton-bool#readme";
  description = "Type level booleans";
  license = lib.licenses.bsd3;
}
