{ mkDerivation, array, base, ghc-prim, HUnit, lib, test-framework
, test-framework-hunit
}:
mkDerivation {
  pname = "deepseq";
  version = "1.4.4.0";
  sha256 = "2ccfc5f83bb3cfc7cce9a8654e664ccac37d4350b1a9661639341d435fbd6e26";
  revision = "1";
  editedCabalFile = "0mbby1hig605jyiyy4m2y2nnjjf5i2adzc6x269hbz4pxscjp43n";
  libraryHaskellDepends = [ array base ];
  testHaskellDepends = [
    array base ghc-prim HUnit test-framework test-framework-hunit
  ];
  description = "Deep evaluation of data structures";
  license = lib.licenses.bsd3;
}
