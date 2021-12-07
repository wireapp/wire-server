{ mkDerivation, base, criterion, exceptions, lib, mmorph, mtl
, optparse-applicative, QuickCheck, test-framework
, test-framework-quickcheck2, transformers, void
}:
mkDerivation {
  pname = "pipes";
  version = "4.3.14";
  sha256 = "db8fcb260537fa34edbd6e72bef979aedd8ebf0b7d14fbb6023e70943c662887";
  libraryHaskellDepends = [
    base exceptions mmorph mtl transformers void
  ];
  testHaskellDepends = [
    base mtl QuickCheck test-framework test-framework-quickcheck2
    transformers
  ];
  benchmarkHaskellDepends = [
    base criterion mtl optparse-applicative transformers
  ];
  description = "Compositional pipelines";
  license = lib.licenses.bsd3;
}
