{ mkDerivation, aeson, base, base-compat, base64-bytestring
, bytestring, constraints, containers, deepseq, exceptions
, fetchgit, free, hspec, hspec-discover, http-media, http-types
, lib, network-uri, QuickCheck, safe, servant, sop-core
, template-haskell, text, transformers
}:
mkDerivation {
  pname = "servant-client-core";
  version = "0.18.3";
  src = fetchgit {
    url = "https://github.com/haskell-servant/servant.git";
    sha256 = "0khgk0iqvamph57qp86ilravaw76qnjmg4kpliwfdzfyj9h44w0l";
    rev = "75db4a5327d6d04ae2460bd5ffd008fe16197ba8";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/servant-client-core; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson base base-compat base64-bytestring bytestring constraints
    containers deepseq exceptions free http-media http-types
    network-uri safe servant sop-core template-haskell text
    transformers
  ];
  testHaskellDepends = [ base base-compat deepseq hspec QuickCheck ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://docs.servant.dev/";
  description = "Core functionality and class for client function generation for servant APIs";
  license = lib.licenses.bsd3;
}
