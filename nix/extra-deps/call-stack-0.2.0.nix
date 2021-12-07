{ mkDerivation, base, lib, nanospec }:
mkDerivation {
  pname = "call-stack";
  version = "0.2.0";
  sha256 = "95c693c93958758d714dee22edc56d4ebb2dcefa4412264e941e32ccd6e4e0c7";
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base nanospec ];
  homepage = "https://github.com/sol/call-stack#readme";
  description = "Use GHC call-stacks in a backward compatible way";
  license = lib.licenses.mit;
}
