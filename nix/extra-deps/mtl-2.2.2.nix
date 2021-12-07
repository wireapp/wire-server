{ mkDerivation, base, lib, transformers }:
mkDerivation {
  pname = "mtl";
  version = "2.2.2";
  sha256 = "8803f48a8ed33296c3a3272f448198737a287ec31baa901af09e2118c829bef6";
  libraryHaskellDepends = [ base transformers ];
  homepage = "http://github.com/haskell/mtl";
  description = "Monad classes, using functional dependencies";
  license = lib.licenses.bsd3;
}
