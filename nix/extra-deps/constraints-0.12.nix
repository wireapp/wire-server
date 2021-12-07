{ mkDerivation, base, binary, deepseq, ghc-prim, hashable, hspec
, hspec-discover, lib, mtl, semigroups, transformers
, transformers-compat, type-equality
}:
mkDerivation {
  pname = "constraints";
  version = "0.12";
  sha256 = "f26b531a15f013f15b4b5df46c4f89b13927253a4195494c50320adf05760223";
  revision = "1";
  editedCabalFile = "109l3ir0jnj6pyf2c761rq6wrpr3j6z0hkv3v9hnf2idm5r1xpvy";
  libraryHaskellDepends = [
    base binary deepseq ghc-prim hashable mtl semigroups transformers
    transformers-compat type-equality
  ];
  testHaskellDepends = [ base hspec ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://github.com/ekmett/constraints/";
  description = "Constraint manipulation";
  license = lib.licenses.bsd2;
}
