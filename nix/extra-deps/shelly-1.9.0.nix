{ mkDerivation, async, base, bytestring, containers, directory
, enclosed-exceptions, exceptions, filepath, hspec, hspec-contrib
, HUnit, lib, lifted-async, lifted-base, monad-control, mtl
, process, text, time, transformers, transformers-base, unix
, unix-compat
}:
mkDerivation {
  pname = "shelly";
  version = "1.9.0";
  sha256 = "5eb5fd4fc105e218cef6cfa10971d299ad660324e6a6006b8cccc31edf39aace";
  revision = "2";
  editedCabalFile = "0d08dlfgwi2m3wvcmcrcsn2g8lpqzkmxwxw74sysh94qb1gasr0y";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base bytestring containers directory enclosed-exceptions
    exceptions filepath lifted-async lifted-base monad-control mtl
    process text time transformers transformers-base unix unix-compat
  ];
  testHaskellDepends = [
    async base bytestring containers directory enclosed-exceptions
    exceptions filepath hspec hspec-contrib HUnit lifted-async
    lifted-base monad-control mtl process text time transformers
    transformers-base unix unix-compat
  ];
  homepage = "https://github.com/yesodweb/Shelly.hs";
  description = "shell-like (systems) programming in Haskell";
  license = lib.licenses.bsd3;
}
