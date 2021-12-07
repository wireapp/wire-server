{ mkDerivation, base, Cabal, directory, lib }:
mkDerivation {
  pname = "ghc-paths";
  version = "0.1.0.12";
  sha256 = "6ecbe676d073cb07989c61ce4c5709c4e67cbefdd2d55a4095f9388b6fe2c484";
  revision = "3";
  editedCabalFile = "109aygfsji4s9sdy1jyh0p5gy2hhcxp3x07fb88f72kfbn6896av";
  setupHaskellDepends = [ base Cabal directory ];
  libraryHaskellDepends = [ base ];
  description = "Knowledge of GHC's installation directories";
  license = lib.licenses.bsd3;
}
