{ mkDerivation, base, bytestring, connection, data-default
, fetchgit, HaskellNet, lib, network, network-bsd
}:
mkDerivation {
  pname = "HaskellNet-SSL";
  version = "0.3.4.4";
  src = fetchgit {
    url = "https://github.com/dpwright/HaskellNet-SSL";
    sha256 = "1a7b20irqf6zkdiaxkg2gf74z9r1llvfv4g2cgsvfrha8smzs20m";
    rev = "c1244053b827fd06faddd84542cdce3ac88b060f";
    fetchSubmodules = true;
  };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base bytestring connection data-default HaskellNet network
    network-bsd
  ];
  homepage = "https://github.com/dpwright/HaskellNet-SSL";
  description = "Helpers to connect to SSL/TLS mail servers with HaskellNet";
  license = lib.licenses.bsd3;
}
