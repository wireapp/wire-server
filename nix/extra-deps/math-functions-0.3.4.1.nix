{ mkDerivation, base, data-default-class, deepseq, erf, gauge, lib
, primitive, QuickCheck, random, tasty, tasty-hunit
, tasty-quickcheck, vector, vector-th-unbox
}:
mkDerivation {
  pname = "math-functions";
  version = "0.3.4.1";
  sha256 = "e20a0afc03d3431610d5f4e57ec3a71822bf6cb1c598e8f2ad1b336533e4a48f";
  libraryHaskellDepends = [
    base data-default-class deepseq primitive vector
  ];
  testHaskellDepends = [
    base data-default-class deepseq erf primitive QuickCheck tasty
    tasty-hunit tasty-quickcheck vector vector-th-unbox
  ];
  benchmarkHaskellDepends = [
    base data-default-class gauge random vector
  ];
  homepage = "https://github.com/bos/math-functions";
  description = "Collection of tools for numeric computations";
  license = lib.licenses.bsd2;
}
