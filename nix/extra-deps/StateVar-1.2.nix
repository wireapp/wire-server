{ mkDerivation, base, lib, stm, transformers }:
mkDerivation {
  pname = "StateVar";
  version = "1.2";
  sha256 = "afc036021fcd38f15fcc4af392a3e57017d5ddcc926e99391dbfc8c4e6375f8b";
  libraryHaskellDepends = [ base stm transformers ];
  homepage = "https://github.com/haskell-opengl/StateVar";
  description = "State variables";
  license = lib.licenses.bsd3;
}
