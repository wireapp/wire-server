{ mkDerivation, base, base-orphans, deepseq, ghc-prim, HUnit, lib
, primitive, QuickCheck, random, semigroups, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, transformers
}:
mkDerivation {
  pname = "vector";
  version = "0.12.1.2";
  sha256 = "3b6e27683f6f20b37b8a35d0fab9e34b57dad72ac64cb16f428726780d4011bd";
  libraryHaskellDepends = [ base deepseq ghc-prim primitive ];
  testHaskellDepends = [
    base base-orphans HUnit primitive QuickCheck random semigroups
    tasty tasty-hunit tasty-quickcheck template-haskell transformers
  ];
  homepage = "https://github.com/haskell/vector";
  description = "Efficient Arrays";
  license = lib.licenses.bsd3;
}
