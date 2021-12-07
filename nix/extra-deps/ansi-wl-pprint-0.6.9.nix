{ mkDerivation, ansi-terminal, base, lib }:
mkDerivation {
  pname = "ansi-wl-pprint";
  version = "0.6.9";
  sha256 = "a7b2e8e7cd3f02f2954e8b17dc60a0ccd889f49e2068ebb15abfa1d42f7a4eac";
  revision = "3";
  editedCabalFile = "1km10sx7ldyv1vfyljik1gqnrwl7bnq2s5m40w41gc930vm48891";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ ansi-terminal base ];
  homepage = "http://github.com/ekmett/ansi-wl-pprint";
  description = "The Wadler/Leijen Pretty Printer for colored ANSI terminal output";
  license = lib.licenses.bsd3;
}
