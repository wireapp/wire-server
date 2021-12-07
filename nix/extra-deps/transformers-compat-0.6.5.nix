{ mkDerivation, base, ghc-prim, lib, transformers }:
mkDerivation {
  pname = "transformers-compat";
  version = "0.6.5";
  sha256 = "da67cf11515da751b32a8ce6e96549f7268f7c435769ad19dc9766b69774620b";
  libraryHaskellDepends = [ base ghc-prim transformers ];
  homepage = "http://github.com/ekmett/transformers-compat/";
  description = "A small compatibility shim for the transformers library";
  license = lib.licenses.bsd3;
}
