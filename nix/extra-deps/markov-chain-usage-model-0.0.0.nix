{ mkDerivation, base, doctest, lib, matrix, tasty, tasty-discover
, tasty-hunit, vector
}:
mkDerivation {
  pname = "markov-chain-usage-model";
  version = "0.0.0";
  sha256 = "4c8e59c753ddcb8a3273c44712fc91e20be7a3f0f3a485f3f9f87003f71a2793";
  libraryHaskellDepends = [ base matrix vector ];
  testHaskellDepends = [
    base doctest matrix tasty tasty-discover tasty-hunit vector
  ];
  testToolDepends = [ tasty-discover ];
  homepage = "https://github.com/advancedtelematic/markov-chain-usage-model#readme";
  description = "Computations for Markov chain usage models";
  license = lib.licenses.bsd2;
}
