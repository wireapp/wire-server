{ mkDerivation, attoparsec, base, blaze-builder, bytestring
, bytestring-conversion, case-insensitive, containers, cookie
, criterion, data-default, fetchgit, http-types, lib, tasty
, tasty-hunit, tasty-quickcheck, transformers, wai, wai-predicates
, wai-route
}:
mkDerivation {
  pname = "wai-routing";
  version = "0.13.0";
  src = fetchgit {
    url = "https://gitlab.com/twittner/wai-routing";
    sha256 = "18icwks9jc6sy42vcvj2ysaip2s0dsrpvm9sy608b6nq6kk1ahlk";
    rev = "7e996a93fec5901767f845a50316b3c18e51a61d";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    attoparsec base bytestring bytestring-conversion case-insensitive
    cookie data-default http-types transformers wai wai-predicates
    wai-route
  ];
  testHaskellDepends = [
    base blaze-builder bytestring bytestring-conversion
    case-insensitive containers http-types tasty tasty-hunit
    tasty-quickcheck wai wai-predicates
  ];
  benchmarkHaskellDepends = [
    base criterion http-types wai wai-predicates
  ];
  homepage = "https://gitlab.com/twittner/wai-routing/";
  description = "Declarative routing for WAI";
  license = lib.licenses.mpl20;
}
