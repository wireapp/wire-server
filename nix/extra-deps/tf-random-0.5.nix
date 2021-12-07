{ mkDerivation, base, lib, primitive, random, time }:
mkDerivation {
  pname = "tf-random";
  version = "0.5";
  sha256 = "2e30cec027b313c9e1794d326635d8fc5f79b6bf6e7580ab4b00186dadc88510";
  libraryHaskellDepends = [ base primitive random time ];
  description = "High-quality splittable pseudorandom number generator";
  license = lib.licenses.bsd3;
}
