{ mkDerivation, base, bytestring, containers, lib, QuickCheck
, template-haskell, text, th-lift, transformers, vector
}:
mkDerivation {
  pname = "th-lift-instances";
  version = "0.1.17";
  sha256 = "45353a114f51614503c3974a062817fd8f878e5650705e5bdc29b3060c91a94c";
  libraryHaskellDepends = [
    base bytestring containers template-haskell text th-lift
    transformers vector
  ];
  testHaskellDepends = [
    base bytestring containers QuickCheck template-haskell text vector
  ];
  homepage = "http://github.com/bennofs/th-lift-instances/";
  description = "Lift instances for template-haskell for common data types";
  license = lib.licenses.bsd3;
}
