{ mkDerivation, base, base-compat, bytestring, case-insensitive
, fetchgit, hpack, hspec, hspec-core, hspec-expectations
, http-types, lib, QuickCheck, text, transformers, wai, wai-extra
}:
mkDerivation {
  pname = "hspec-wai";
  version = "0.9.0";
  src = fetchgit {
    url = "https://github.com/wireapp/hspec-wai";
    sha256 = "1dxw7phnvz48v63jwiz9v1fb29zf75r8rgb8v4mc2yghnnpm15i7";
    rev = "791dcbb48dff41735b53758d988b8754ad7dc49b";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base base-compat bytestring case-insensitive hspec-core
    hspec-expectations http-types QuickCheck text transformers wai
    wai-extra
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base base-compat bytestring case-insensitive hspec hspec-core
    hspec-expectations http-types QuickCheck text transformers wai
    wai-extra
  ];
  prePatch = "hpack";
  homepage = "https://github.com/hspec/hspec-wai#readme";
  description = "Experimental Hspec support for testing WAI applications";
  license = lib.licenses.mit;
}
