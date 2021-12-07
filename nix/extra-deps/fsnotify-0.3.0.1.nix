{ mkDerivation, async, base, bytestring, containers, directory
, filepath, hinotify, lib, random, shelly, tasty, tasty-hunit
, temporary, text, time, unix, unix-compat
}:
mkDerivation {
  pname = "fsnotify";
  version = "0.3.0.1";
  sha256 = "ded2165f72a2b4971f941cb83ef7f58b200e3e04159be78da55ba6c5d35f6da5";
  revision = "1";
  editedCabalFile = "1pa9pa0kflkqlb4dysagy0aihn452hmf6wwlsr7fp4ygg86m7fsq";
  libraryHaskellDepends = [
    async base bytestring containers directory filepath hinotify shelly
    text time unix unix-compat
  ];
  testHaskellDepends = [
    async base directory filepath random tasty tasty-hunit temporary
    unix-compat
  ];
  homepage = "https://github.com/haskell-fswatch/hfsnotify";
  description = "Cross platform library for file change notification";
  license = lib.licenses.bsd3;
}
