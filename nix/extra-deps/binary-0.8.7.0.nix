{ mkDerivation, array, attoparsec, base, base-orphans, bytestring
, Cabal, cereal, containers, criterion, deepseq, directory
, filepath, generic-deriving, HUnit, lib, mtl, QuickCheck, random
, test-framework, test-framework-quickcheck2, unordered-containers
, zlib
}:
mkDerivation {
  pname = "binary";
  version = "0.8.7.0";
  sha256 = "1d9defe3fe53ffde464c9715928e5614e613c470f0a1ade55f19d648d2f349e4";
  revision = "1";
  editedCabalFile = "029fyhy8qra8mg52pn9jvh3lsnabxr3a8qbgqnb9h705p36ifm32";
  libraryHaskellDepends = [ array base bytestring containers ];
  testHaskellDepends = [
    array base base-orphans bytestring Cabal containers directory
    filepath HUnit QuickCheck random test-framework
    test-framework-quickcheck2
  ];
  benchmarkHaskellDepends = [
    array attoparsec base bytestring cereal containers criterion
    deepseq directory filepath generic-deriving mtl
    unordered-containers zlib
  ];
  homepage = "https://github.com/kolmodin/binary";
  description = "Binary serialisation for Haskell values using lazy ByteStrings";
  license = lib.licenses.bsd3;
}
