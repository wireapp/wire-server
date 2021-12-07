{ mkDerivation, base, containers, lib, QuickCheck, tagged
, transformers
}:
mkDerivation {
  pname = "universe-base";
  version = "1.1.1";
  sha256 = "1ded30b31b1abbc7621bdb0086ba0d91c1920157e87a4abeb4f2fcf9f3f6dea8";
  revision = "2";
  editedCabalFile = "0601hqv5h2274i11j1ai2yqb7zk2fkqkiqlpwnq5awbgdkwb10i8";
  libraryHaskellDepends = [ base containers tagged transformers ];
  testHaskellDepends = [ base containers QuickCheck ];
  homepage = "https://github.com/dmwit/universe";
  description = "A class for finite and recursively enumerable types";
  license = lib.licenses.bsd3;
}
