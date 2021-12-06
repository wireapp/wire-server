{ mkDerivation, aeson, base, base-compat, bytestring, containers
, deepseq, entropy, exceptions, fetchgit, hspec, hspec-discover
, http-api-data, http-client, http-media, http-types, HUnit
, kan-extensions, lib, markdown-unlit, monad-control, mtl, network
, QuickCheck, semigroupoids, servant, servant-client-core
, servant-server, stm, tdigest, text, time, transformers
, transformers-base, transformers-compat, wai, warp
}:
mkDerivation {
  pname = "servant-client";
  version = "0.16";
  src = fetchgit {
    url = "https://github.com/wireapp/servant";
    sha256 = "104mshjyp45ibdii6xsxw9dv5hs51vqhy60mqlid8qfxgzj36639";
    rev = "ad0228030fb6e0d213b66f5e5f98447da9c3a9a7";
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
    network QuickCheck servant servant-client-core servant-server stm
    tdigest text transformers transformers-compat wai warp
  ];
  testToolDepends = [ hspec-discover markdown-unlit ];
  homepage = "http://docs.servant.dev/";
  description = "Automatic derivation of querying functions for servant";
  license = lib.licenses.bsd3;
}
