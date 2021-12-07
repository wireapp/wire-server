{ mkDerivation, base, bytestring, Cabal, directory, filepath, lib
, process, unix
}:
mkDerivation {
  pname = "entropy";
  version = "0.4.1.6";
  sha256 = "adc759ff756a6d71a450422ba511177632f43a33bf673901fd2334f53ef8bf62";
  revision = "1";
  editedCabalFile = "0jyyyn1cfi1cjisbgym90nw2vx2082ghvwg54ibnjzbii9aj7fj9";
  setupHaskellDepends = [ base Cabal directory filepath process ];
  libraryHaskellDepends = [ base bytestring unix ];
  homepage = "https://github.com/TomMD/entropy";
  description = "A platform independent entropy source";
  license = lib.licenses.bsd3;
}
