{ mkDerivation, base, lib, mtl, transformers, transformers-compat
}:
mkDerivation {
  pname = "mmorph";
  version = "1.1.3";
  sha256 = "7923f7ad6260a05aaa8175b9f2a250f5bb63187427681171bd36d29a6cf2da65";
  libraryHaskellDepends = [
    base mtl transformers transformers-compat
  ];
  description = "Monad morphisms";
  license = lib.licenses.bsd3;
}
