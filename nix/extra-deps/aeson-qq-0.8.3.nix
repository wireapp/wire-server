{ mkDerivation, aeson, attoparsec, base, base-compat, ghc-prim
, haskell-src-meta, hspec, lib, parsec, scientific
, template-haskell, text, vector
}:
mkDerivation {
  pname = "aeson-qq";
  version = "0.8.3";
  sha256 = "8f3129cf88bf52214a9f74c0be584a3c3296d1541280ad900188e102fee7f482";
  libraryHaskellDepends = [
    aeson attoparsec base base-compat haskell-src-meta parsec
    scientific template-haskell text vector
  ];
  testHaskellDepends = [
    aeson attoparsec base base-compat ghc-prim haskell-src-meta hspec
    parsec scientific template-haskell text vector
  ];
  homepage = "https://github.com/sol/aeson-qq#readme";
  description = "JSON quasiquoter for Haskell";
  license = lib.licenses.mit;
}
