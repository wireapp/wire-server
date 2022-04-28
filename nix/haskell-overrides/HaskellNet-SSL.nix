{ mkDerivation, base, bytestring, connection, data-default
, fetchgit, HaskellNet, lib, network, network-bsd, tls
}:
mkDerivation {
  pname = "HaskellNet-SSL";
  version = "0.3.4.2";
  src = fetchgit {
    url = "https://github.com/dpwright/HaskellNet-SSL";
    sha256 = "1w23xgjdq22px90p12yw30psagc668n7l183bqvf8x075s77wckr";
    rev = "ca84ef29a93eaef7673fa58056cdd8dae1568d2d";
    fetchSubmodules = true;
  };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base bytestring connection data-default HaskellNet network
    network-bsd tls
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/dpwright/HaskellNet-SSL";
  description = "Helpers to connect to SSL/TLS mail servers with HaskellNet";
  license = lib.licenses.bsd3;
}
