{ mkDerivation, base, data-default-class
, data-default-instances-containers, data-default-instances-dlist
, data-default-instances-old-locale, lib
}:
mkDerivation {
  pname = "data-default";
  version = "0.7.1.1";
  sha256 = "b0f95d279cd75cacaa8152a01590dc3460f7134f6840b37052abb3ba3cb2a511";
  libraryHaskellDepends = [
    base data-default-class data-default-instances-containers
    data-default-instances-dlist data-default-instances-old-locale
  ];
  description = "A class for types with a default value";
  license = lib.licenses.bsd3;
}
