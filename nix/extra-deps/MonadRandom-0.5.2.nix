{ mkDerivation, base, lib, mtl, primitive, random, transformers
, transformers-compat
}:
mkDerivation {
  pname = "MonadRandom";
  version = "0.5.2";
  sha256 = "603806756bb51391feab2bc0d690facb9654283643beb5f94a4bbce6ae8651e6";
  revision = "1";
  editedCabalFile = "02spi82dyap56pb5pdn5rjqn4q8sr11n7rmmy0qq1ib2mgx9jni4";
  libraryHaskellDepends = [
    base mtl primitive random transformers transformers-compat
  ];
  description = "Random-number generation monad";
  license = lib.licenses.bsd3;
}
