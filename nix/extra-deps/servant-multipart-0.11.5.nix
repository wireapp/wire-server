{ mkDerivation, array, base, bytestring, directory, http-client
, http-media, lens, lib, network, random, resourcet, servant
, servant-client, servant-client-core, servant-docs
, servant-foreign, servant-server, text, transformers, wai
, wai-extra, warp
}:
mkDerivation {
  pname = "servant-multipart";
  version = "0.11.5";
  sha256 = "0afe0afbe1463c6f178c9441f6a840a48d53481a0817fec1e8176c17fc007fa5";
  libraryHaskellDepends = [
    array base bytestring directory http-media lens random resourcet
    servant servant-client-core servant-docs servant-foreign
    servant-server text transformers wai wai-extra
  ];
  testHaskellDepends = [
    base bytestring http-client network servant servant-client
    servant-client-core servant-server text transformers wai warp
  ];
  homepage = "https://github.com/haskell-servant/servant-multipart#readme";
  description = "multipart/form-data (e.g file upload) support for servant";
  license = lib.licenses.bsd3;
}
