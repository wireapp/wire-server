{ mkDerivation, ansi-terminal, base, bytestring, containers, Diff
, dlist, exceptions, filepath, ghc-lib-parser, gitrev, hspec
, hspec-discover, lib, mtl, optparse-applicative, path, path-io
, syb, text
}:
mkDerivation {
  pname = "ormolu";
  version = "0.1.4.1";
  sha256 = "3ab5bb2e6a9de89cdedd9c2adfab45a0b722d7735225bff83c305959e37f55a9";
  revision = "1";
  editedCabalFile = "1fi8fxyhw9jdwhsbmrikjqd461wrz7h4kdszrahlvdjfdsn4wh7d";
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    ansi-terminal base bytestring containers Diff dlist exceptions
    ghc-lib-parser mtl syb text
  ];
  executableHaskellDepends = [
    base filepath ghc-lib-parser gitrev optparse-applicative text
  ];
  testHaskellDepends = [
    base containers filepath hspec path path-io text
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/tweag/ormolu";
  description = "A formatter for Haskell source code";
  license = lib.licenses.bsd3;
}
