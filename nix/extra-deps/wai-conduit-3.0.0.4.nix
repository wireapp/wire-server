{ mkDerivation, base, bytestring, conduit, http-types, lib
, transformers, wai
}:
mkDerivation {
  pname = "wai-conduit";
  version = "3.0.0.4";
  sha256 = "2790093bd52892b8087c295044573c720773144f4061ccc72d6d6a617320d61f";
  libraryHaskellDepends = [
    base bytestring conduit http-types transformers wai
  ];
  homepage = "https://github.com/yesodweb/wai";
  description = "conduit wrappers for WAI";
  license = lib.licenses.mit;
}
