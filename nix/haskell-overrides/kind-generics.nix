{ mkDerivation, base, kind-apply, lib }:
mkDerivation {
  pname = "kind-generics";
  version = "0.4.1.0";
  sha256 = "dde6ba1f5aef32fceaae6e0466ce9cb2a3822beade59e0efa2bd0ebd7389aed1";
  libraryHaskellDepends = [ base kind-apply ];
  doHaddock = false;
  doCheck = false;
  description = "Generic programming in GHC style for arbitrary kinds and GADTs";
  license = lib.licenses.bsd3;
}
