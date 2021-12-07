{ mkDerivation, base, hxt-charproperties, lib }:
mkDerivation {
  pname = "hxt-unicode";
  version = "9.0.2.4";
  sha256 = "7b5823f3bd94b57022d9d84ab3555303653c5121eaaef2ee1fd4918f3c434466";
  libraryHaskellDepends = [ base hxt-charproperties ];
  homepage = "https://github.com/UweSchmidt/hxt";
  description = "Unicode en-/decoding functions for utf8, iso-latin-* and other encodings";
  license = lib.licenses.mit;
}
