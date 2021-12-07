{ mkDerivation, atomic-primops, base, bytestring, clock, containers
, criterion, deepseq, doctest, exceptions, hspec, lib, mtl
, QuickCheck, random, random-shuffle, stm, text, transformers
, transformers-compat, utf8-string
}:
mkDerivation {
  pname = "prometheus-client";
  version = "1.0.1";
  sha256 = "36c2c6322bc4f70f635909f32d0965fa6ce5ebbb177482b83f783613e8a7ccbf";
  libraryHaskellDepends = [
    atomic-primops base bytestring clock containers deepseq exceptions
    mtl stm text transformers transformers-compat utf8-string
  ];
  testHaskellDepends = [
    atomic-primops base bytestring clock containers deepseq doctest
    exceptions hspec mtl QuickCheck random-shuffle stm text
    transformers transformers-compat utf8-string
  ];
  benchmarkHaskellDepends = [
    base bytestring criterion random text utf8-string
  ];
  homepage = "https://github.com/fimad/prometheus-haskell";
  description = "Haskell client library for http://prometheus.io.";
  license = lib.licenses.asl20;
}
