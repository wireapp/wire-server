{ mkDerivation, base, deepseq, inspection-testing, lib, QuickCheck
}:
mkDerivation {
  pname = "generic-random";
  version = "1.3.0.1";
  sha256 = "f3342eb7a071f945f8a2ed9041a20f9449a51797a5a5ec4a5a612c5d593f3c35";
  revision = "1";
  editedCabalFile = "0h3nd6kz5brwigbx25gqzry4nr82y0ww3pyh1nx24xrpdpx6s39c";
  libraryHaskellDepends = [ base QuickCheck ];
  testHaskellDepends = [
    base deepseq inspection-testing QuickCheck
  ];
  homepage = "http://github.com/lysxia/generic-random";
  description = "Generic random generators for QuickCheck";
  license = lib.licenses.mit;
}
