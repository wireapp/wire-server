{ mkDerivation, base, lib, MonadRandom, random }:
mkDerivation {
  pname = "random-shuffle";
  version = "0.0.4";
  sha256 = "52704411f040fd0bf2361dad162e35dc13caa6535b2e4908d3513c00a95d0615";
  libraryHaskellDepends = [ base MonadRandom random ];
  description = "Random shuffle implementation";
  license = lib.licenses.bsd3;
}
