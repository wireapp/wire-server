{ mkDerivation, base, bytestring, http-types, lib
, streaming-commons, wai
}:
mkDerivation {
  pname = "wai-middleware-gunzip";
  version = "0.0.2";
  sha256 = "bd9d542ee4b1255c01161266da2bcf7056111ae77f792a23ac2263e209bf7b65";
  libraryHaskellDepends = [
    base bytestring http-types streaming-commons wai
  ];
  homepage = "https://github.com/twittner/wai-middleware-gunzip";
  description = "WAI middleware to unzip request bodies";
  license = lib.licenses.mpl20;
}
