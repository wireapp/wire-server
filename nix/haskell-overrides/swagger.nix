{ mkDerivation, aeson, base, bytestring, fetchgit, lib, tasty
, tasty-hunit, text, time, transformers
}:
mkDerivation {
  pname = "swagger";
  version = "0.3.0";
  src = fetchgit {
    url = "https://gitlab.com/axeman/swagger";
    sha256 = "1zj3fqlvcvp9s0myb98b6s9mpmiqamyxn2d3jan55irdgm53prmv";
    rev = "e2d3f5b5274b8d8d301b5377b0af4319cea73f9e";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson base bytestring text time transformers
  ];
  testHaskellDepends = [ aeson base bytestring tasty tasty-hunit ];
  doHaddock = false;
  doCheck = false;
  description = "Implementation of swagger data model";
  license = "unknown";
}
