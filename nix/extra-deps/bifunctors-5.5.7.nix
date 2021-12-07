{ mkDerivation, base, base-orphans, comonad, containers, hspec
, hspec-discover, lib, QuickCheck, tagged, template-haskell
, th-abstraction, transformers, transformers-compat
}:
mkDerivation {
  pname = "bifunctors";
  version = "5.5.7";
  sha256 = "88b3a2d4504e1139a3aef7027913faa0870631477d0a2ebb6fa67d494cdb3532";
  revision = "2";
  editedCabalFile = "0w7fscgxin56pcl2p04gbvd5g2y5pk0dg1cx9qrxwshn1x30gn8r";
  libraryHaskellDepends = [
    base base-orphans comonad containers tagged template-haskell
    th-abstraction transformers
  ];
  testHaskellDepends = [
    base hspec QuickCheck template-haskell transformers
    transformers-compat
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://github.com/ekmett/bifunctors/";
  description = "Bifunctors";
  license = lib.licenses.bsd3;
}
