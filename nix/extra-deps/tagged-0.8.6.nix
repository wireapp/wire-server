{ mkDerivation, base, deepseq, lib, template-haskell, transformers
}:
mkDerivation {
  pname = "tagged";
  version = "0.8.6";
  sha256 = "ad16def0884cf6f05ae1ae8e90192cf9d8d9673fa264b249499bd9e4fac791dd";
  revision = "3";
  editedCabalFile = "1wv9ngbj3pvs6v52dj0bli9h5vanyw3akpsmfmwsvnnian9hpkkw";
  libraryHaskellDepends = [
    base deepseq template-haskell transformers
  ];
  homepage = "http://github.com/ekmett/tagged";
  description = "Haskell 98 phantom types to avoid unsafely passing dummy arguments";
  license = lib.licenses.bsd3;
}
