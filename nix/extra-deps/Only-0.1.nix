{ mkDerivation, base, deepseq, lib }:
mkDerivation {
  pname = "Only";
  version = "0.1";
  sha256 = "ab7aa193e8c257d3bda6b0b3c1cbcf74cdaa85ab08cb20c2dd62ba248c1ab265";
  revision = "1";
  editedCabalFile = "1ahk7p34kmh041mz7lyc10nhcxgv2i4z8nvzxvqm2x34gslmsbzr";
  libraryHaskellDepends = [ base deepseq ];
  description = "The 1-tuple type or single-value \"collection\"";
  license = lib.licenses.bsd3;
}
