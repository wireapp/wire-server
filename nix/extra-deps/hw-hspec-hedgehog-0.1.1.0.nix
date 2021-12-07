{ mkDerivation, base, call-stack, doctest, doctest-discover
, hedgehog, hspec, hspec-discover, HUnit, lib, transformers
}:
mkDerivation {
  pname = "hw-hspec-hedgehog";
  version = "0.1.1.0";
  sha256 = "d6cfd42f59482b153b276b2bc897320c6b633be87e0eea78649e184316042313";
  revision = "3";
  editedCabalFile = "0byjlgisygdak9pf9dfnpbj576mrjd7knx4kyfm12l6l5qhcw8n1";
  libraryHaskellDepends = [
    base call-stack hedgehog hspec HUnit transformers
  ];
  testHaskellDepends = [
    base call-stack doctest doctest-discover hedgehog hspec HUnit
  ];
  testToolDepends = [ doctest-discover hspec-discover ];
  homepage = "https://github.com/haskell-works/hw-hspec-hedgehog#readme";
  description = "Interoperability between hspec and hedgehog";
  license = lib.licenses.bsd3;
}
