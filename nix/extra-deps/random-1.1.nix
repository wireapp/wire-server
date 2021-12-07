{ mkDerivation, base, lib, time }:
mkDerivation {
  pname = "random";
  version = "1.1";
  sha256 = "b718a41057e25a3a71df693ab0fe2263d492e759679b3c2fea6ea33b171d3a5a";
  revision = "1";
  editedCabalFile = "1pv5d7bm2rgap7llp5vjsplrg048gvf0226y0v19gpvdsx7n4rvv";
  libraryHaskellDepends = [ base time ];
  testHaskellDepends = [ base ];
  description = "random number library";
  license = lib.licenses.bsd3;
}
