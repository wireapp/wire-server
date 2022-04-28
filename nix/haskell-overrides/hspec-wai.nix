{ mkDerivation, base, base-compat, bytestring, case-insensitive
, fetchgit, hspec, hspec-core, hspec-expectations, http-types, lib
, QuickCheck, text, transformers, wai, wai-extra
}:
mkDerivation {
  pname = "hspec-wai";
  version = "0.9.2";
  src = fetchgit {
    url = "https://github.com/wireapp/hspec-wai";
    sha256 = "1yqkla7467fgb23yw689xh15zjn38rkc7ckf18cfalpc2ff5wfq1";
    rev = "0a5142cd3ba48116ff059c041348b817fb7bdb25";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base base-compat bytestring case-insensitive hspec-core
    hspec-expectations http-types QuickCheck text transformers wai
    wai-extra
  ];
  testHaskellDepends = [
    base base-compat bytestring case-insensitive hspec hspec-core
    hspec-expectations http-types QuickCheck text transformers wai
    wai-extra
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/hspec/hspec-wai#readme";
  description = "Experimental Hspec support for testing WAI applications";
  license = lib.licenses.mit;
}
