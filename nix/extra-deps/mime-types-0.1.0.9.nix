{ mkDerivation, base, bytestring, containers, lib, text }:
mkDerivation {
  pname = "mime-types";
  version = "0.1.0.9";
  sha256 = "0a32435169ef4ba59f4a4b8addfd0c04479410854d1b8d69a1e38fb389ba71d2";
  libraryHaskellDepends = [ base bytestring containers text ];
  homepage = "https://github.com/yesodweb/wai";
  description = "Basic mime-type handling types and functions";
  license = lib.licenses.mit;
}
