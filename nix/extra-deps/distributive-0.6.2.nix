{ mkDerivation, base, base-orphans, Cabal, cabal-doctest, doctest
, generic-deriving, hspec, hspec-discover, lib, tagged
, transformers
}:
mkDerivation {
  pname = "distributive";
  version = "0.6.2";
  sha256 = "824ee271ded433ce6c61fe890c784642b7e20ffa4082fca609da54e7dcfc23c9";
  revision = "2";
  editedCabalFile = "11y4qanr09880d7sxkhrzrh3cpmqbx001hiqhsr2s75xrnmrpkia";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [ base base-orphans tagged transformers ];
  testHaskellDepends = [ base doctest generic-deriving hspec ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://github.com/ekmett/distributive/";
  description = "Distributive functors -- Dual to Traversable";
  license = lib.licenses.bsd3;
}
