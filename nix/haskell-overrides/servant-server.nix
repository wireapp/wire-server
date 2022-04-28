{ mkDerivation, aeson, base, base-compat, base64-bytestring
, bytestring, constraints, containers, directory, exceptions
, fetchgit, filepath, hspec, hspec-discover, hspec-wai
, http-api-data, http-media, http-types, lib, monad-control, mtl
, network, network-uri, QuickCheck, resourcet, safe, servant
, should-not-typecheck, sop-core, string-conversions, tagged
, temporary, text, transformers, transformers-base
, transformers-compat, wai, wai-app-static, wai-extra, warp, word8
}:
mkDerivation {
  pname = "servant-server";
  version = "0.18.3";
  src = fetchgit {
    url = "https://github.com/haskell-servant/servant.git";
    sha256 = "0khgk0iqvamph57qp86ilravaw76qnjmg4kpliwfdzfyj9h44w0l";
    rev = "75db4a5327d6d04ae2460bd5ffd008fe16197ba8";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/servant-server; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base-compat base64-bytestring bytestring constraints
    containers exceptions filepath http-api-data http-media http-types
    monad-control mtl network network-uri resourcet servant sop-core
    string-conversions tagged text transformers transformers-base wai
    wai-app-static word8
  ];
  executableHaskellDepends = [
    aeson base base-compat servant text wai warp
  ];
  testHaskellDepends = [
    aeson base base-compat base64-bytestring bytestring directory hspec
    hspec-wai http-types mtl QuickCheck resourcet safe servant
    should-not-typecheck sop-core string-conversions temporary text
    transformers transformers-compat wai wai-extra
  ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://docs.servant.dev/";
  description = "A family of combinators for defining webservices APIs and serving them";
  license = lib.licenses.bsd3;
  mainProgram = "greet";
}
