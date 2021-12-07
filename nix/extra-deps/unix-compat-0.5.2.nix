{ mkDerivation, base, lib, unix }:
mkDerivation {
  pname = "unix-compat";
  version = "0.5.2";
  sha256 = "659a4e442f71505d45b0d0fb28a347aa1ac5e1f39feaea5d58e674e5d2ce0ba9";
  revision = "2";
  editedCabalFile = "1zhxkqbvvlnrqqx21kfb03h786za3fk4x3kh236sgv0hhkdf3271";
  libraryHaskellDepends = [ base unix ];
  homepage = "http://github.com/jacobstanley/unix-compat";
  description = "Portable POSIX-compatibility layer";
  license = lib.licenses.bsd3;
}
