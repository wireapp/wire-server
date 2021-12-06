{ mkDerivation, base, filepath, hpack, imports, lib
, optparse-applicative, shelly, system-filepath, text
}:
mkDerivation {
  pname = "makedeb";
  version = "0.3.0";
  src = /home/axeman/workspace/wire-server/tools/makedeb;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base filepath imports optparse-applicative shelly system-filepath
    text
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base imports optparse-applicative ];
  prePatch = "hpack";
  description = "Create debian archives with dpkg-deb";
  license = lib.licenses.agpl3Only;
}
