{ mkDerivation, aeson, base, bytestring, lib }:
mkDerivation {
  pname = "deriving-aeson";
  version = "0.2.5";
  sha256 = "8c01298ad2e54700e8a106244fb21d3387c803a7403e7f8a811a3895466079df";
  revision = "1";
  editedCabalFile = "0k7wvz2a24afzfcrk2k2k56v15kx2vzxwf87yblam91yxj2g224c";
  libraryHaskellDepends = [ aeson base ];
  testHaskellDepends = [ aeson base bytestring ];
  description = "Type driven generic aeson instance customisation";
  license = lib.licenses.bsd3;
}
