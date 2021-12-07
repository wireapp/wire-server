{ mkDerivation, base, blaze-markup, bytestring, http-media, lib
, servant, servant-blaze, servant-server, swagger2, text
, transformers, transformers-compat, wai-app-static
}:
mkDerivation {
  pname = "servant-swagger-ui-core";
  version = "0.3.3";
  sha256 = "03724a312f08d9f59893cf8a55be719219c0490bdd3fc0f83359ca459995ed3e";
  revision = "4";
  editedCabalFile = "1kj72caf8skcl25bfw4nh7kj7hh33wd55sggsd474di57wi9j2c4";
  libraryHaskellDepends = [
    base blaze-markup bytestring http-media servant servant-blaze
    servant-server swagger2 text transformers transformers-compat
    wai-app-static
  ];
  homepage = "https://github.com/haskell-servant/servant-swagger-ui";
  description = "Servant swagger ui core components";
  license = lib.licenses.bsd3;
}
