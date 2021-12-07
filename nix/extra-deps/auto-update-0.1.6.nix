{ mkDerivation, base, exceptions, hspec, HUnit, lib, retry }:
mkDerivation {
  pname = "auto-update";
  version = "0.1.6";
  sha256 = "f4e023dc8713c387ecf20d851247597fd012cabea3872310b35e911105eb66c4";
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base exceptions hspec HUnit retry ];
  homepage = "https://github.com/yesodweb/wai";
  description = "Efficiently run periodic, on-demand actions";
  license = lib.licenses.mit;
}
