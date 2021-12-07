{ mkDerivation, base, haskell-src-meta, invariant, lens, lib
, partial-isomorphisms, QuickCheck, semigroupoids, template-haskell
, transformers
}:
mkDerivation {
  pname = "invertible";
  version = "0.2.0.7";
  sha256 = "311e9bb0ca4c22955f02ab410e614608df685e7f4421cb5a2c2f7b968aafecd9";
  revision = "1";
  editedCabalFile = "19xcczz26ji5xaws4ikvacqz991qgislj32hs8rlks07qw3qmnbn";
  libraryHaskellDepends = [
    base haskell-src-meta invariant lens partial-isomorphisms
    semigroupoids template-haskell transformers
  ];
  testHaskellDepends = [ base QuickCheck transformers ];
  description = "bidirectional arrows, bijective functions, and invariant functors";
  license = lib.licenses.bsd3;
}
