{ mkDerivation, base, lens, lib, time }:
mkDerivation {
  pname = "lens-datetime";
  version = "0.3";
  sha256 = "bb1f8d7bf71c9ef8901bc39e2a2d629b1101307115c0c4d844fcbd8e86b6ccd4";
  libraryHaskellDepends = [ base lens time ];
  homepage = "https://github.com/nilcons/lens-datetime";
  description = "Lenses for Data.Time.* types";
  license = lib.licenses.bsd3;
}
