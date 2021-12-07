{ mkDerivation, base, lib, text }:
mkDerivation {
  pname = "mime";
  version = "0.4.0.2";
  sha256 = "e7fa283e5caa6371d5b3978e152412a2dd2489eec43aee722caca9dc14ea3875";
  revision = "1";
  editedCabalFile = "07r4kyjm2bk8knyhbfivgxlxnxp7qqlcnzp61f2hi7d1s7clg290";
  libraryHaskellDepends = [ base text ];
  homepage = "https://github.com/GaloisInc/mime";
  description = "Working with MIME types";
  license = lib.licenses.bsd3;
}
