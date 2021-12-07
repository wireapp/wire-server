{ mkDerivation, base, bytestring, case-insensitive, file-embed
, http-types, lib, network, text, transformers, wai, wai-app-static
, warp, websockets
}:
mkDerivation {
  pname = "wai-websockets";
  version = "3.0.1.2";
  sha256 = "917cceb08f296d7dc6b6cafb66133ae53888b2c98b8fb2a2d7fa629d75ab5d2c";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring case-insensitive http-types network transformers
    wai websockets
  ];
  executableHaskellDepends = [
    base bytestring case-insensitive file-embed http-types network text
    transformers wai wai-app-static warp websockets
  ];
  homepage = "http://github.com/yesodweb/wai";
  description = "Provide a bridge between WAI and the websockets package";
  license = lib.licenses.mit;
}
