{ mkDerivation, base, deepseq, directory, lib, nanospec, temporary
}:
mkDerivation {
  pname = "silently";
  version = "1.2.5.1";
  sha256 = "7fc9baf6f47ffc082e7e05c9dade1451bdee06a0c4e2d882e8e0b692f50bfad1";
  libraryHaskellDepends = [ base deepseq directory ];
  testHaskellDepends = [ base deepseq directory nanospec temporary ];
  homepage = "https://github.com/hspec/silently";
  description = "Prevent or capture writing to stdout and other handles";
  license = lib.licenses.bsd3;
}
