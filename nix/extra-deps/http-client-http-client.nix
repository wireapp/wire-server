{ mkDerivation, array, async, base, blaze-builder, bytestring
, case-insensitive, containers, cookie, deepseq, directory
, exceptions, fetchgit, filepath, ghc-prim, hspec, http-types, lib
, memory, mime-types, monad-control, network, network-uri, random
, stm, streaming-commons, text, time, transformers, zlib
}:
mkDerivation {
  pname = "http-client";
  version = "0.7.0";
  src = fetchgit {
    url = "https://github.com/wireapp/http-client";
    sha256 = "16n340bg5vdb169f6d6421hx13wyqdsb5b314r823v34r8p0b19z";
    rev = "9100baeddbd15d93dc58a826ae812dafff29d5fd";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/http-client; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    array base blaze-builder bytestring case-insensitive containers
    cookie deepseq exceptions filepath ghc-prim http-types memory
    mime-types network network-uri random stm streaming-commons text
    time transformers
  ];
  testHaskellDepends = [
    async base blaze-builder bytestring case-insensitive containers
    cookie deepseq directory hspec http-types monad-control network
    network-uri streaming-commons text time transformers zlib
  ];
  doCheck = false;
  homepage = "https://github.com/snoyberg/http-client";
  description = "An HTTP client engine";
  license = lib.licenses.mit;
}
