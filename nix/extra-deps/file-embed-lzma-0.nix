{ mkDerivation, base, base-compat, bytestring, directory, filepath
, lib, lzma, template-haskell, text, th-lift-instances
, transformers
}:
mkDerivation {
  pname = "file-embed-lzma";
  version = "0";
  sha256 = "e86cf44f747cf403898158e9fdf9342871e293097a29679fcf587aed497f0c77";
  revision = "7";
  editedCabalFile = "1jm3jr70vvfv9an3nb7n5rx5ldk6i4c1dcwi3pgbf6lkx7lkp754";
  libraryHaskellDepends = [
    base base-compat bytestring directory filepath lzma
    template-haskell text th-lift-instances transformers
  ];
  testHaskellDepends = [ base bytestring ];
  homepage = "https://github.com/phadej/file-embed-lzma";
  description = "Use Template Haskell to embed (LZMA compressed) data";
  license = lib.licenses.bsd3;
}
