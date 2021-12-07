{ mkDerivation, base, lib, primitive, stm }:
mkDerivation {
  pname = "primitive-unlifted";
  version = "0.1.2.0";
  sha256 = "031932b9958adabcb27ef1017a42266f3589b44cd25338c4d41d6a31407705ff";
  libraryHaskellDepends = [ base primitive ];
  testHaskellDepends = [ base primitive stm ];
  homepage = "https://github.com/haskell-primitive/primitive-unlifted";
  description = "Primitive GHC types with unlifted types inside";
  license = lib.licenses.bsd3;
}
