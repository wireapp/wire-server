{ mkDerivation, base, bytestring, deepseq, lib }:
mkDerivation {
  pname = "bytestring-builder";
  version = "0.10.8.2.0";
  sha256 = "27faef6db27c5be5a3715fd68b93725853e0e668849eaf92ce7c33cef9cb2c3f";
  libraryHaskellDepends = [ base bytestring deepseq ];
  doHaddock = false;
  description = "The new bytestring builder, packaged outside of GHC";
  license = lib.licenses.bsd3;
}
