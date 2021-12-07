{ mkDerivation, base, call-stack, HUnit, lib, nanospec }:
mkDerivation {
  pname = "hspec-expectations";
  version = "0.8.2";
  sha256 = "819607ea1faf35ce5be34be61c6f50f3389ea43892d56fb28c57a9f5d54fb4ef";
  libraryHaskellDepends = [ base call-stack HUnit ];
  testHaskellDepends = [ base call-stack HUnit nanospec ];
  homepage = "https://github.com/hspec/hspec-expectations#readme";
  description = "Catchy combinators for HUnit";
  license = lib.licenses.mit;
}
