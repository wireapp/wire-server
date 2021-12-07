{ mkDerivation, base, colour, lib }:
mkDerivation {
  pname = "ansi-terminal";
  version = "0.10.3";
  sha256 = "fa2e2e32dfad29835aa7fd442bbe233e07d97e933223a001fe5efa562535b57c";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base colour ];
  homepage = "https://github.com/feuerbach/ansi-terminal";
  description = "Simple ANSI terminal support, with Windows compatibility";
  license = lib.licenses.bsd3;
}
