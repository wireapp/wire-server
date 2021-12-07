{ mkDerivation, auto-update, base, lib }:
mkDerivation {
  pname = "time-manager";
  version = "0.0.0";
  sha256 = "90a616ed20b2119bb64f78f84230b6798cde22a35e87bc8d9ee08cdf1d90fcdb";
  libraryHaskellDepends = [ auto-update base ];
  homepage = "http://github.com/yesodweb/wai";
  description = "Scalable timer";
  license = lib.licenses.mit;
}
