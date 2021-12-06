{ mkDerivation, aeson, aeson-pretty, base, base-compat, bytestring
, Cabal, cabal-doctest, directory, doctest, fetchgit, filepath
, hspec, hspec-discover, http-media, insert-ordered-containers
, lens, lens-aeson, lib, QuickCheck, servant, singleton-bool
, swagger2, template-haskell, text, time, unordered-containers
, utf8-string, vector
}:
mkDerivation {
  pname = "servant-swagger";
  version = "1.1.11";
  src = fetchgit {
    url = "https://github.com/haskell-servant/servant-swagger";
    sha256 = "1zrv19bnyqwkv8rv5wqaivrhq5n1bzfp8wn06qlzzdicvi591lhf";
    rev = "7ac475a40b4b95a50bc2094f9461c62bd7fbb1fd";
    fetchSubmodules = true;
  };
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    aeson aeson-pretty base base-compat bytestring hspec http-media
    insert-ordered-containers lens QuickCheck servant singleton-bool
    swagger2 text unordered-containers
  ];
  testHaskellDepends = [
    aeson base base-compat directory doctest filepath hspec lens
    lens-aeson QuickCheck servant swagger2 template-haskell text time
    utf8-string vector
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/haskell-servant/servant-swagger";
  description = "Generate a Swagger/OpenAPI/OAS 2.0 specification for your servant API.";
  license = lib.licenses.bsd3;
}
