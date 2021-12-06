{ mkDerivation, aeson, base, base-compat, base64-bytestring
, bytestring, containers, deepseq, exceptions, fetchgit, free
, hspec, hspec-discover, http-media, http-types, lib, network-uri
, QuickCheck, safe, servant, template-haskell, text, transformers
}:
mkDerivation {
  pname = "servant-client-core";
  version = "0.16";
  src = fetchgit {
    url = "https://github.com/wireapp/servant";
    sha256 = "104mshjyp45ibdii6xsxw9dv5hs51vqhy60mqlid8qfxgzj36639";
    rev = "ad0228030fb6e0d213b66f5e5f98447da9c3a9a7";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/servant-client-core; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson base base-compat base64-bytestring bytestring containers
    deepseq exceptions free http-media http-types network-uri safe
    servant template-haskell text transformers
  ];
  testHaskellDepends = [ base base-compat deepseq hspec QuickCheck ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://docs.servant.dev/";
  description = "Core functionality and class for client function generation for servant APIs";
  license = lib.licenses.bsd3;
}
