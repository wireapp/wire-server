{ mkDerivation, base, bytestring, doctest, lib }:
mkDerivation {
  pname = "network-byte-order";
  version = "0.1.5";
  sha256 = "ccf8b8475e7563de72a20e673fe55c32b6d3550ee6004b585bc13175bc560011";
  revision = "1";
  editedCabalFile = "0vrsz93bapgc8z3ny8ch65j1x1vj4f7pim9lk19rr806syla01g3";
  libraryHaskellDepends = [ base bytestring ];
  testHaskellDepends = [ base bytestring doctest ];
  description = "Network byte order utilities";
  license = lib.licenses.bsd3;
}
