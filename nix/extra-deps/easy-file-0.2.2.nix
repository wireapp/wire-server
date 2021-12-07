{ mkDerivation, base, directory, filepath, lib, time, unix }:
mkDerivation {
  pname = "easy-file";
  version = "0.2.2";
  sha256 = "52f52e72ba48d60935932401c233a72bf45c582871238aecc5a18021ce67b47e";
  libraryHaskellDepends = [ base directory filepath time unix ];
  homepage = "http://github.com/kazu-yamamoto/easy-file";
  description = "Cross-platform File handling";
  license = lib.licenses.bsd3;
}
