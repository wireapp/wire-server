{ mkDerivation, aeson, attoparsec, base, base-compat, bifunctors
, bytestring, case-insensitive, constraints, deepseq, fetchgit
, hspec, hspec-discover, http-api-data, http-media, http-types, lib
, mmorph, mtl, network-uri, QuickCheck, quickcheck-instances
, singleton-bool, sop-core, string-conversions, tagged, text
, transformers, vault
}:
mkDerivation {
  pname = "servant";
  version = "0.18.3";
  src = fetchgit {
    url = "https://github.com/haskell-servant/servant.git";
    sha256 = "0khgk0iqvamph57qp86ilravaw76qnjmg4kpliwfdzfyj9h44w0l";
    rev = "75db4a5327d6d04ae2460bd5ffd008fe16197ba8";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/servant; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson attoparsec base base-compat bifunctors bytestring
    case-insensitive constraints deepseq http-api-data http-media
    http-types mmorph mtl network-uri QuickCheck singleton-bool
    sop-core string-conversions tagged text transformers vault
  ];
  testHaskellDepends = [
    aeson base base-compat bytestring hspec http-media mtl QuickCheck
    quickcheck-instances string-conversions text transformers
  ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://docs.servant.dev/";
  description = "A family of combinators for defining webservices APIs";
  license = lib.licenses.bsd3;
}
