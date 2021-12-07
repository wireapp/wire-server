{ mkDerivation, base, HUnit, lib, template-haskell }:
mkDerivation {
  pname = "raw-strings-qq";
  version = "1.1";
  sha256 = "2e011ec26aeaa53ab43c30b7d9b5b0f661f24b4ebef8884c12c571353c0fbed3";
  libraryHaskellDepends = [ base template-haskell ];
  testHaskellDepends = [ base HUnit ];
  homepage = "https://github.com/23Skidoo/raw-strings-qq";
  description = "Raw string literals for Haskell";
  license = lib.licenses.bsd3;
}
