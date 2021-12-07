{ mkDerivation, base, basement, gauge, ghc-prim, lib }:
mkDerivation {
  pname = "foundation";
  version = "0.0.25";
  sha256 = "c158550f8429be8614e832aa0b5018b29a4e5a8429118bdca5f6d7e74fe9d360";
  revision = "1";
  editedCabalFile = "1ps5sk50sf4b5hd87k3jqykqrwcw2wzyp50rcy6pghd61h83cjg2";
  libraryHaskellDepends = [ base basement ghc-prim ];
  testHaskellDepends = [ base basement ];
  benchmarkHaskellDepends = [ base basement gauge ];
  homepage = "https://github.com/haskell-foundation/foundation";
  description = "Alternative prelude with batteries and no dependencies";
  license = lib.licenses.bsd3;
}
