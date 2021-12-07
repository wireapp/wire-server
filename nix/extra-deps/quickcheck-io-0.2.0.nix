{ mkDerivation, base, HUnit, lib, QuickCheck }:
mkDerivation {
  pname = "quickcheck-io";
  version = "0.2.0";
  sha256 = "fb779119d79fe08ff4d502fb6869a70c9a8d5fd8ae0959f605c3c937efd96422";
  libraryHaskellDepends = [ base HUnit QuickCheck ];
  homepage = "https://github.com/hspec/quickcheck-io#readme";
  description = "Use HUnit assertions as QuickCheck properties";
  license = lib.licenses.mit;
}
