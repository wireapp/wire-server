{ mkDerivation, aeson, base, base-compat, bytestring
, bytestring-conversion, hspec, hspec-discover, hspec-wai
, http-types, lib, QuickCheck, servant, servant-server
, transformers, wai, warp
}:
mkDerivation {
  pname = "servant-mock";
  version = "0.8.7";
  sha256 = "dd48c4c64eae5ea24d579eadc726ed4acc75a5a5d99174e139344b1b164f0c49";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base-compat bytestring http-types QuickCheck servant
    servant-server transformers wai
  ];
  executableHaskellDepends = [
    aeson base QuickCheck servant-server warp
  ];
  testHaskellDepends = [
    aeson base bytestring-conversion hspec hspec-wai QuickCheck servant
    servant-server wai
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "Derive a mock server for free from your servant API types";
  license = lib.licenses.bsd3;
}
