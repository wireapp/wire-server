{ mkDerivation, base, lib, transformers }:
mkDerivation {
  pname = "unliftio-core";
  version = "0.1.2.0";
  sha256 = "24c38b3d610ca2642ed496d1de3d7b6b398ce0410aa0a15f3c7ce636ba8f7a78";
  revision = "3";
  editedCabalFile = "08ybg150pa1h1d5z6qqicfv8yfjpc3mrjp70xdn4la72zhpnb9xh";
  libraryHaskellDepends = [ base transformers ];
  homepage = "https://github.com/fpco/unliftio/tree/master/unliftio-core#readme";
  description = "The MonadUnliftIO typeclass for unlifting monads to IO";
  license = lib.licenses.mit;
}
