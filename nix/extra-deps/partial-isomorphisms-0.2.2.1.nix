{ mkDerivation, base, lib, template-haskell }:
mkDerivation {
  pname = "partial-isomorphisms";
  version = "0.2.2.1";
  sha256 = "4c551fa69119e87de1ba0ec7b854f6ed13fb2fe2768db4afff2f8468f0f4a164";
  libraryHaskellDepends = [ base template-haskell ];
  homepage = "http://www.informatik.uni-marburg.de/~rendel/unparse";
  description = "Partial isomorphisms";
  license = lib.licenses.bsd3;
}
