{ mkDerivation, base, lib }:
mkDerivation {
  pname = "network-info";
  version = "0.2.0.10";
  sha256 = "5680f6975d34cf4f81fa7ca0c8efd682261d6a1119e06dece0f67c7bd97fd52a";
  revision = "1";
  editedCabalFile = "07kiw56lhc56kqrnvpa11f5nnnid6by3aq00jrkcbbg7w0q71a6d";
  libraryHaskellDepends = [ base ];
  homepage = "http://github.com/jacobstanley/network-info";
  description = "Access the local computer's basic network configuration";
  license = lib.licenses.bsd3;
}
