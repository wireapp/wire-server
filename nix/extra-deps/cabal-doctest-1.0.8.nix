{ mkDerivation, base, Cabal, directory, filepath, lib }:
mkDerivation {
  pname = "cabal-doctest";
  version = "1.0.8";
  sha256 = "2026a6a87d410202ce091412ca6bc33c5aca787025326b4a3d13425a23392e0e";
  revision = "2";
  editedCabalFile = "05v1awad3d1wvc763xcgvxm4n6n7bs7byc6s14kdbw35zcaddlcb";
  libraryHaskellDepends = [ base Cabal directory filepath ];
  homepage = "https://github.com/phadej/cabal-doctest";
  description = "A Setup.hs helper for doctests running";
  license = lib.licenses.bsd3;
}
