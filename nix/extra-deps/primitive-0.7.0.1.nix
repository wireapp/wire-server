{ mkDerivation, base, base-orphans, ghc-prim, lib, QuickCheck
, semigroups, tagged, tasty, tasty-quickcheck, transformers
, transformers-compat
}:
mkDerivation {
  pname = "primitive";
  version = "0.7.0.1";
  sha256 = "dd8bb7d829f492d6200fca4a839a7563b80571c5f76b4d0fa76700ebe897f7dd";
  libraryHaskellDepends = [ base ghc-prim transformers ];
  testHaskellDepends = [
    base base-orphans ghc-prim QuickCheck semigroups tagged tasty
    tasty-quickcheck transformers transformers-compat
  ];
  homepage = "https://github.com/haskell/primitive";
  description = "Primitive memory-related operations";
  license = lib.licenses.bsd3;
}
