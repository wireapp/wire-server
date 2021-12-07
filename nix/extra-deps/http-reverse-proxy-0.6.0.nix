{ mkDerivation, base, blaze-builder, bytestring, case-insensitive
, conduit, conduit-extra, containers, hspec, http-client
, http-conduit, http-types, lib, network, resourcet
, streaming-commons, text, transformers, unliftio, wai, wai-logger
, warp, word8
}:
mkDerivation {
  pname = "http-reverse-proxy";
  version = "0.6.0";
  sha256 = "fb1c913111478384c4f23647810b8c3c01c79e9276a08a1ea46215e4a42dd1a8";
  libraryHaskellDepends = [
    base blaze-builder bytestring case-insensitive conduit
    conduit-extra containers http-client http-types network resourcet
    streaming-commons text transformers unliftio wai wai-logger word8
  ];
  testHaskellDepends = [
    base blaze-builder bytestring conduit conduit-extra hspec
    http-conduit http-types network resourcet streaming-commons
    transformers unliftio wai warp
  ];
  homepage = "https://github.com/fpco/http-reverse-proxy";
  description = "Reverse proxy HTTP requests, either over raw sockets or with WAI";
  license = lib.licenses.bsd3;
}
