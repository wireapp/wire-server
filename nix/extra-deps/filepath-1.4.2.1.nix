{ mkDerivation, base, lib, QuickCheck }:
mkDerivation {
  pname = "filepath";
  version = "1.4.2.1";
  sha256 = "0b43631506cf6d851cb6a100e4d00cd7f658727555687ddfa4db91a594625412";
  revision = "2";
  editedCabalFile = "0jpmcdm852wr18kliyp0a69iij3v9rp6wx92gkkk2nc9vns3mwdg";
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base QuickCheck ];
  homepage = "https://github.com/haskell/filepath#readme";
  description = "Library for manipulating FilePaths in a cross platform way";
  license = lib.licenses.bsd3;
}
