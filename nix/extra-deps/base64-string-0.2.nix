{ mkDerivation, base, lib }:
mkDerivation {
  pname = "base64-string";
  version = "0.2";
  sha256 = "3ec896ca7261ad4ddeffbaa3bdf4d5cb61775250c303fca9929aa9a56acc705e";
  libraryHaskellDepends = [ base ];
  homepage = "http://urchin.earth.li/~ian/cabal/base64-string/";
  description = "Base64 implementation for String's";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
