{ mkDerivation, base, bytestring, fetchgit, HsOpenSSL, hspec
, http-client, http-types, lib, network
}:
mkDerivation {
  pname = "http-client-openssl";
  version = "0.3.1.0";
  src = fetchgit {
    url = "https://github.com/wireapp/http-client";
    sha256 = "16n340bg5vdb169f6d6421hx13wyqdsb5b314r823v34r8p0b19z";
    rev = "9100baeddbd15d93dc58a826ae812dafff29d5fd";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/http-client-openssl; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring HsOpenSSL http-client network
  ];
  testHaskellDepends = [
    base HsOpenSSL hspec http-client http-types
  ];
  doCheck = false;
  homepage = "https://github.com/snoyberg/http-client";
  description = "http-client backend using the OpenSSL library";
  license = lib.licenses.mit;
}
