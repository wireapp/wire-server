{ mkDerivation, base, exceptions, ghc-prim, hedgehog, HUnit, lib
, mtl, random, stm, tasty, tasty-hedgehog, tasty-hunit, time
, transformers
}:
mkDerivation {
  pname = "retry";
  version = "0.8.1.2";
  sha256 = "c5415ed7928d81611fa570fef9dd6c009f3d722a16a36f1177bdde2e888e9e5b";
  libraryHaskellDepends = [
    base exceptions ghc-prim random transformers
  ];
  testHaskellDepends = [
    base exceptions ghc-prim hedgehog HUnit mtl random stm tasty
    tasty-hedgehog tasty-hunit time transformers
  ];
  homepage = "http://github.com/Soostone/retry";
  description = "Retry combinators for monadic actions that may fail";
  license = lib.licenses.bsd3;
}
