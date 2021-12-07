{ mkDerivation, base, filepath, lib, time, unix }:
mkDerivation {
  pname = "directory";
  version = "1.3.6.0";
  sha256 = "ae6e0f07aa245a43964a37ce680985b3535552995c63b2e1607f703d8c82afbb";
  libraryHaskellDepends = [ base filepath time unix ];
  testHaskellDepends = [ base filepath time unix ];
  description = "Platform-agnostic library for filesystem operations";
  license = lib.licenses.bsd3;
}
