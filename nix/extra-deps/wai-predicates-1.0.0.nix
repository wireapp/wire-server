{ mkDerivation, attoparsec, base, blaze-builder, bytestring
, bytestring-conversion, case-insensitive, cookie, http-types, lib
, singletons, tasty, tasty-hunit, tasty-quickcheck, text
, transformers, vault, vector, wai
}:
mkDerivation {
  pname = "wai-predicates";
  version = "1.0.0";
  sha256 = "514a758b1d563126b9503e53da37b9baaff4a1a6718217d0652f1fe6d4c5ca54";
  libraryHaskellDepends = [
    attoparsec base bytestring bytestring-conversion case-insensitive
    cookie http-types singletons text transformers vault vector wai
  ];
  testHaskellDepends = [
    base blaze-builder bytestring case-insensitive http-types tasty
    tasty-hunit tasty-quickcheck wai
  ];
  homepage = "https://gitlab.com/twittner/wai-predicates/";
  description = "WAI request predicates";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
