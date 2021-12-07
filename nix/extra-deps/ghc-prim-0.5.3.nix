{ mkDerivation, lib, rts }:
mkDerivation {
  pname = "ghc-prim";
  version = "0.5.3";
  sha256 = "6957ee95288ed6ea9522ef909acdebba8be31e44e6eefbff41770ce9892e471f";
  libraryHaskellDepends = [ rts ];
  description = "GHC primitives";
  license = lib.licenses.bsd3;
}
