{ mkDerivation, aeson, base, base-compat, bytestring, containers
, deepseq, entropy, exceptions, fetchgit, hspec, hspec-discover
, http-api-data, http-client, http-media, http-types, HUnit
, kan-extensions, lib, markdown-unlit, monad-control, mtl, network
, QuickCheck, semigroupoids, servant, servant-client-core
, servant-server, sop-core, stm, tdigest, text, time, transformers
, transformers-base, transformers-compat, wai, warp
}:
mkDerivation {
  pname = "servant-client";
  version = "0.18.3";
  src = fetchgit {
    url = "https://github.com/haskell-servant/servant.git";
    sha256 = "0khgk0iqvamph57qp86ilravaw76qnjmg4kpliwfdzfyj9h44w0l";
    rev = "75db4a5327d6d04ae2460bd5ffd008fe16197ba8";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/servant-client; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base base-compat bytestring containers deepseq exceptions
    http-client http-media http-types kan-extensions monad-control mtl
    semigroupoids servant servant-client-core stm text time
    transformers transformers-base transformers-compat
  ];
  testHaskellDepends = [
    aeson base base-compat bytestring entropy hspec http-api-data
    http-client http-types HUnit kan-extensions markdown-unlit mtl
    network QuickCheck servant servant-client-core servant-server
    sop-core stm tdigest text transformers transformers-compat wai warp
  ];
  testToolDepends = [ hspec-discover markdown-unlit ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://docs.servant.dev/";
  description = "Automatic derivation of querying functions for servant";
  license = lib.licenses.bsd3;
}
