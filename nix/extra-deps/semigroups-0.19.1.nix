{ mkDerivation, base, lib }:
mkDerivation {
  pname = "semigroups";
  version = "0.19.1";
  sha256 = "79e761e64b862564a3470d5d356cb6b060b14452d675859aed3b2d1e14646648";
  revision = "2";
  editedCabalFile = "0xm3diaq83pyvgmgw25vx2d1c3sc914kly0sxg3vch54n1j4npxa";
  libraryHaskellDepends = [ base ];
  homepage = "http://github.com/ekmett/semigroups/";
  description = "Anything that associates";
  license = lib.licenses.bsd3;
}
