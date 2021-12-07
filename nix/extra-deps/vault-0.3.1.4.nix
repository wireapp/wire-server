{ mkDerivation, base, containers, hashable, lib, semigroups
, unordered-containers
}:
mkDerivation {
  pname = "vault";
  version = "0.3.1.4";
  sha256 = "5b1cd255de8f4addf7705f7b9bfd586b003bcace0dba0dbe1ae1986b8a0d4359";
  libraryHaskellDepends = [
    base containers hashable semigroups unordered-containers
  ];
  homepage = "https://github.com/HeinrichApfelmus/vault";
  description = "a persistent store for values of arbitrary types";
  license = lib.licenses.bsd3;
}
