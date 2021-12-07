{ mkDerivation, array, base, ghc-prim, integer-gmp, lib, QuickCheck
, smallcheck, tasty, tasty-hunit, tasty-quickcheck
, tasty-smallcheck
}:
mkDerivation {
  pname = "integer-logarithms";
  version = "1.0.3";
  sha256 = "5ae262018698af35bb74916fad170d96d3eb44669c72ed36db9a19a3392cec16";
  revision = "2";
  editedCabalFile = "0a6j3313vz7n7dn8abddyib4jggblaq89f87ib4imdwjxjajbm33";
  libraryHaskellDepends = [ array base ghc-prim integer-gmp ];
  testHaskellDepends = [
    base QuickCheck smallcheck tasty tasty-hunit tasty-quickcheck
    tasty-smallcheck
  ];
  homepage = "https://github.com/Bodigrim/integer-logarithms";
  description = "Integer logarithms";
  license = lib.licenses.mit;
}
