{ mkDerivation, base, filepath, imports, lib, optparse-applicative
, shelly, system-filepath, text
}:
mkDerivation {
  pname = "makedeb";
  version = "0.3.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base filepath imports optparse-applicative shelly system-filepath
    text
  ];
  executableHaskellDepends = [ base imports optparse-applicative ];
  description = "Create debian archives with dpkg-deb";
  license = lib.licenses.agpl3Only;
}
