{ mkDerivation, base, binary, bytestring, hspec, hspec-discover
, lib, old-locale, old-time, QuickCheck, time
}:
mkDerivation {
  pname = "unix-time";
  version = "0.4.7";
  sha256 = "19233f8badf921d444c6165689253d877cfed58ce08f28cad312558a9280de09";
  libraryHaskellDepends = [ base binary bytestring old-time ];
  testHaskellDepends = [
    base bytestring hspec old-locale old-time QuickCheck time
  ];
  testToolDepends = [ hspec-discover ];
  description = "Unix time parser/formatter and utilities";
  license = lib.licenses.bsd3;
}
