{ mkDerivation, ghc-prim, lib }:
mkDerivation {
  pname = "integer-gmp";
  version = "1.0.2.0";
  sha256 = "d368b822451554e4ae779c98746a4dd6ed0645faf1320fa1ea2e2f5e5b155fa9";
  libraryHaskellDepends = [ ghc-prim ];
  description = "Integer library based on GMP";
  license = lib.licenses.bsd3;
}
