{ mkDerivation, base, ghc, lib }:
mkDerivation {
  pname = "ghc-tcplugins-extra";
  version = "0.4";
  sha256 = "1c6d4ffde539e3ff427c8f83d803cc1a4ddb6dd54d8fbc92318fcc9387aa057d";
  libraryHaskellDepends = [ base ghc ];
  homepage = "http://github.com/clash-lang/ghc-tcplugins-extra";
  description = "Utilities for writing GHC type-checker plugins";
  license = lib.licenses.bsd2;
}
