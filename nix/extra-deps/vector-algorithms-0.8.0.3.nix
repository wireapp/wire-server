{ mkDerivation, base, bytestring, containers, lib, mwc-random
, primitive, QuickCheck, vector
}:
mkDerivation {
  pname = "vector-algorithms";
  version = "0.8.0.3";
  sha256 = "1ac41f014663fd318b34a76b80e6d8f32f1629ef4996ae7304f31597a0d07387";
  libraryHaskellDepends = [ base bytestring primitive vector ];
  testHaskellDepends = [
    base bytestring containers QuickCheck vector
  ];
  benchmarkHaskellDepends = [ base mwc-random vector ];
  homepage = "https://github.com/erikd/vector-algorithms/";
  description = "Efficient algorithms for vector arrays";
  license = lib.licenses.bsd3;
}
