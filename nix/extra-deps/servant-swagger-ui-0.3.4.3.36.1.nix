{ mkDerivation, base, bytestring, file-embed-lzma, lib, servant
, servant-server, servant-swagger-ui-core, swagger2, text
}:
mkDerivation {
  pname = "servant-swagger-ui";
  version = "0.3.4.3.36.1";
  sha256 = "16f976c8612791f57efce3949d92f4279a83aa361d82d74fd961d33a4a64c648";
  libraryHaskellDepends = [
    base bytestring file-embed-lzma servant servant-server
    servant-swagger-ui-core swagger2 text
  ];
  homepage = "https://github.com/haskell-servant/servant-swagger-ui";
  description = "Servant swagger ui";
  license = lib.licenses.bsd3;
}
