{ mkDerivation, base, deepseq, lib }:
mkDerivation {
  pname = "data-checked";
  version = "0.3";
  sha256 = "dc87d09c7c8587c9e6e372166e8de3b42c2cd804a493ff100c253e4d713c5676";
  libraryHaskellDepends = [ base deepseq ];
  homepage = "https://github.com/mvv/data-checked";
  description = "Type-indexed runtime-checked properties";
  license = lib.licenses.bsd3;
}
