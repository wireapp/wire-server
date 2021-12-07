{ mkDerivation, base, lib, math-functions, primitive, time, vector
}:
mkDerivation {
  pname = "mwc-random";
  version = "0.14.0.0";
  sha256 = "00370edaa60a51c86663868ecc2b1995824970001875cec458e9acc13511efa2";
  libraryHaskellDepends = [
    base math-functions primitive time vector
  ];
  doCheck = false;
  homepage = "https://github.com/bos/mwc-random";
  description = "Fast, high quality pseudo random number generation";
  license = lib.licenses.bsd3;
}
