{ mkDerivation, base, bytestring, lib }:
mkDerivation {
  pname = "utf8-string";
  version = "1.0.1.1";
  sha256 = "fb0b9e3acbe0605bcd1c63e51f290a7bbbe6628dfa3294ff453e4235fbaef140";
  revision = "3";
  editedCabalFile = "02vhj5gykkqa2dyn7s6gn8is1b5fdn9xcqqvlls268g7cpv6rk38";
  libraryHaskellDepends = [ base bytestring ];
  homepage = "http://github.com/glguy/utf8-string/";
  description = "Support for reading and writing UTF8 Strings";
  license = lib.licenses.bsd3;
}
