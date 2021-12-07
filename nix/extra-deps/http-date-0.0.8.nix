{ mkDerivation, array, attoparsec, base, bytestring, doctest, hspec
, lib, old-locale, time
}:
mkDerivation {
  pname = "http-date";
  version = "0.0.8";
  sha256 = "0f4c6348487abe4f9d58e43d3c23bdefc7fd1fd5672effd3c7d84aaff05f5427";
  revision = "1";
  editedCabalFile = "1za6cjv6hk9357n874jxhh962brcmvgflc07013598wqs6psya9b";
  libraryHaskellDepends = [ array attoparsec base bytestring time ];
  testHaskellDepends = [
    base bytestring doctest hspec old-locale time
  ];
  description = "HTTP Date parser/formatter";
  license = lib.licenses.bsd3;
}
