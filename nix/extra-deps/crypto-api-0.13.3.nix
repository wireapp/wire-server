{ mkDerivation, base, bytestring, cereal, entropy, lib, tagged
, transformers
}:
mkDerivation {
  pname = "crypto-api";
  version = "0.13.3";
  sha256 = "298a9ea7ce97c8ccf4bfe46d4864092c3a007a56bede73560070db3bf1ac7aa5";
  revision = "1";
  editedCabalFile = "1z6n1sa5pn3iqvqjrd8hv4bc2pxzsrhm5sh0l8z7g9lbqp6w0wp5";
  libraryHaskellDepends = [
    base bytestring cereal entropy tagged transformers
  ];
  homepage = "https://github.com/TomMD/crypto-api";
  description = "A generic interface for cryptographic operations";
  license = lib.licenses.bsd3;
}
