{ mkDerivation, base, blaze-html, http-media, lib, servant
, servant-server, wai, warp
}:
mkDerivation {
  pname = "servant-blaze";
  version = "0.9";
  sha256 = "79981f35382b8dc0bd0492a1362f6bbb3e77ff72649cc09ab513c5530dbbd6dd";
  revision = "3";
  editedCabalFile = "0pn9ca2jmx71clz0j9nlz1lwmr2xv39zqfda10al11am9mc4j8n4";
  libraryHaskellDepends = [ base blaze-html http-media servant ];
  testHaskellDepends = [ base blaze-html servant-server wai warp ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "Blaze-html support for servant";
  license = lib.licenses.bsd3;
}
