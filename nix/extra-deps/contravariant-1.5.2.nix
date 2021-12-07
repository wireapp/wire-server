{ mkDerivation, base, lib, StateVar, transformers }:
mkDerivation {
  pname = "contravariant";
  version = "1.5.2";
  sha256 = "c4262c24e3dcc2ba8ca221ed52a6390818a715301e4f13135d8d732e0c7dc60c";
  libraryHaskellDepends = [ base StateVar transformers ];
  homepage = "http://github.com/ekmett/contravariant/";
  description = "Contravariant functors";
  license = lib.licenses.bsd3;
}
