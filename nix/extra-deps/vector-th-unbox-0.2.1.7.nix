{ mkDerivation, base, data-default, lib, template-haskell, vector
}:
mkDerivation {
  pname = "vector-th-unbox";
  version = "0.2.1.7";
  sha256 = "d125830ff3287c2d7e24989a3887987107b9140912c326e5894fb0b097c50d61";
  revision = "1";
  editedCabalFile = "11qhhir9cdy3x7pd0z0xk8vi4nzr9fn9q3ggwbhhc43jglngw1x7";
  libraryHaskellDepends = [ base template-haskell vector ];
  testHaskellDepends = [ base data-default vector ];
  description = "Deriver for Data.Vector.Unboxed using Template Haskell";
  license = lib.licenses.bsd3;
}
