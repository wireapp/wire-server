{ mkDerivation, base, byteorder, bytestring, Cabal, cabal-doctest
, doctest, fast-logger, http-types, lib, network, wai
}:
mkDerivation {
  pname = "wai-logger";
  version = "2.3.6";
  sha256 = "e2fbd8c74fa0a31f9ea0faa53f4ad4e588644a34d8dfc7cc50d85c245c3c7541";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    base byteorder bytestring fast-logger http-types network wai
  ];
  testHaskellDepends = [ base doctest ];
  description = "A logging system for WAI";
  license = lib.licenses.bsd3;
}
