{ mkDerivation, base, lib }:
mkDerivation {
  pname = "void";
  version = "0.7.3";
  sha256 = "53af758ddc37dc63981671e503438d02c6f64a2d8744e9bec557a894431f7317";
  libraryHaskellDepends = [ base ];
  homepage = "http://github.com/ekmett/void";
  description = "A Haskell 98 logically uninhabited data type";
  license = lib.licenses.bsd3;
}
