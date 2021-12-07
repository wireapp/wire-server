{ mkDerivation, base, lib, old-locale }:
mkDerivation {
  pname = "old-time";
  version = "1.1.0.3";
  sha256 = "1ccb158b0f7851715d36b757c523b026ca1541e2030d02239802ba39b4112bc1";
  revision = "2";
  editedCabalFile = "1j6ln1dkvhdvnwl33bp0xf9lhc4sybqk0aw42p8cq81xwwzbn7y9";
  libraryHaskellDepends = [ base old-locale ];
  description = "Time library";
  license = lib.licenses.bsd3;
}
