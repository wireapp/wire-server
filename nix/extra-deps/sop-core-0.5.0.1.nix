{ mkDerivation, base, deepseq, lib }:
mkDerivation {
  pname = "sop-core";
  version = "0.5.0.1";
  sha256 = "dac367f1608c9bd6c5dd1697e2a30e1b760617023b96e1df7d44c6c017999db0";
  revision = "1";
  editedCabalFile = "1d4sagrlhmvai3f4hvb9rn8aqsjbvi00z0mzv1gds9nblshk83xd";
  libraryHaskellDepends = [ base deepseq ];
  description = "True Sums of Products";
  license = lib.licenses.bsd3;
}
