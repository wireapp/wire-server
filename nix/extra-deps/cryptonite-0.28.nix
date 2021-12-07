{ mkDerivation, base, basement, bytestring, deepseq, gauge
, ghc-prim, integer-gmp, lib, memory, random, tasty, tasty-hunit
, tasty-kat, tasty-quickcheck
}:
mkDerivation {
  pname = "cryptonite";
  version = "0.28";
  sha256 = "74ad886ae3f7cd6cadecb596707e49df37b0170ceed313e382bd15b13132a5db";
  revision = "1";
  editedCabalFile = "0hqbpdsj1b4fgisr11a9gmin5r7bqr3f83wc2xxc18dr01xhrw7d";
  libraryHaskellDepends = [
    base basement bytestring deepseq ghc-prim integer-gmp memory
  ];
  testHaskellDepends = [
    base bytestring memory tasty tasty-hunit tasty-kat tasty-quickcheck
  ];
  benchmarkHaskellDepends = [
    base bytestring deepseq gauge memory random
  ];
  homepage = "https://github.com/haskell-crypto/cryptonite";
  description = "Cryptography Primitives sink";
  license = lib.licenses.bsd3;
}
