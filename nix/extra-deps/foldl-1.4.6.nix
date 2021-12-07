{ mkDerivation, base, bytestring, comonad, containers
, contravariant, criterion, doctest, hashable, lib, mwc-random
, primitive, profunctors, semigroupoids, semigroups, text
, transformers, unordered-containers, vector, vector-builder
}:
mkDerivation {
  pname = "foldl";
  version = "1.4.6";
  sha256 = "2a14aae99eb30344c983b8a07e0c2e5999d097042951e9349eb32d0f388a04aa";
  libraryHaskellDepends = [
    base bytestring comonad containers contravariant hashable
    mwc-random primitive profunctors semigroupoids semigroups text
    transformers unordered-containers vector vector-builder
  ];
  testHaskellDepends = [ base doctest ];
  benchmarkHaskellDepends = [ base criterion ];
  description = "Composable, streaming, and efficient left folds";
  license = lib.licenses.bsd3;
}
