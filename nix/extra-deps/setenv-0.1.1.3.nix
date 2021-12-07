{ mkDerivation, base, lib, unix }:
mkDerivation {
  pname = "setenv";
  version = "0.1.1.3";
  sha256 = "e358df39afc03d5a39e2ec650652d845c85c80cc98fe331654deafb4767ecb32";
  revision = "1";
  editedCabalFile = "0ny4g3kjys0hqg41mnwrsymy1bwhl8l169kis4y4fa58sb06m4f5";
  libraryHaskellDepends = [ base unix ];
  description = "A cross-platform library for setting environment variables";
  license = lib.licenses.mit;
}
