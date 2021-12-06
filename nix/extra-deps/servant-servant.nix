{ mkDerivation, aeson, attoparsec, base, base-compat, bifunctors
, bytestring, Cabal, cabal-doctest, case-insensitive, deepseq
, doctest, fetchgit, hspec, hspec-discover, http-api-data
, http-media, http-types, lib, mmorph, mtl, network-uri, QuickCheck
, quickcheck-instances, singleton-bool, string-conversions, tagged
, text, transformers, vault
}:
mkDerivation {
  pname = "servant";
  version = "0.16";
  src = fetchgit {
    url = "https://github.com/wireapp/servant";
    sha256 = "104mshjyp45ibdii6xsxw9dv5hs51vqhy60mqlid8qfxgzj36639";
    rev = "ad0228030fb6e0d213b66f5e5f98447da9c3a9a7";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/servant; echo source root reset to $sourceRoot";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    aeson attoparsec base base-compat bifunctors bytestring
    case-insensitive deepseq http-api-data http-media http-types mmorph
    mtl network-uri QuickCheck singleton-bool string-conversions tagged
    text transformers vault
  ];
  testHaskellDepends = [
    aeson base base-compat bytestring doctest hspec mtl QuickCheck
    quickcheck-instances string-conversions text transformers
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://docs.servant.dev/";
  description = "A family of combinators for defining webservices APIs";
  license = lib.licenses.bsd3;
}
