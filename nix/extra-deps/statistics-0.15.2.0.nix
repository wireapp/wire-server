{ mkDerivation, aeson, async, base, base-orphans, binary
, data-default-class, deepseq, dense-linear-algebra, erf, ieee754
, lib, math-functions, monad-par, mwc-random, primitive, QuickCheck
, tasty, tasty-expected-failure, tasty-hunit, tasty-quickcheck
, vector, vector-algorithms, vector-binary-instances
, vector-th-unbox
}:
mkDerivation {
  pname = "statistics";
  version = "0.15.2.0";
  sha256 = "c496dbb8767a65ea3c352fd08ce1918200a0cc9d8f8b5f262aebbb43dee22a49";
  libraryHaskellDepends = [
    aeson async base base-orphans binary data-default-class deepseq
    dense-linear-algebra math-functions monad-par mwc-random primitive
    vector vector-algorithms vector-binary-instances vector-th-unbox
  ];
  testHaskellDepends = [
    aeson base binary dense-linear-algebra erf ieee754 math-functions
    mwc-random primitive QuickCheck tasty tasty-expected-failure
    tasty-hunit tasty-quickcheck vector vector-algorithms
  ];
  homepage = "https://github.com/bos/statistics";
  description = "A library of statistical types, data, and functions";
  license = lib.licenses.bsd2;
}
