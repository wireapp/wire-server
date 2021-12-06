{ mkDerivation, base, bytestring, case-insensitive, connection
, containers, cryptonite, data-default-class, exceptions, fetchgit
, gauge, hspec, http-client, http-types, lib, memory, network
, network-uri, text, tls, transformers
}:
mkDerivation {
  pname = "http-client-tls";
  version = "0.3.5.3";
  src = fetchgit {
    url = "https://github.com/wireapp/http-client";
    sha256 = "16n340bg5vdb169f6d6421hx13wyqdsb5b314r823v34r8p0b19z";
    rev = "9100baeddbd15d93dc58a826ae812dafff29d5fd";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/http-client-tls; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring case-insensitive connection containers cryptonite
    data-default-class exceptions http-client http-types memory network
    network-uri text tls transformers
  ];
  testHaskellDepends = [
    base connection hspec http-client http-types
  ];
  benchmarkHaskellDepends = [ base gauge http-client ];
  doCheck = false;
  homepage = "https://github.com/snoyberg/http-client";
  description = "http-client backend using the connection package and tls library";
  license = lib.licenses.mit;
}
