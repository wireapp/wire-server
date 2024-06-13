{ mkDerivation, aeson, algebraic-graphs, async, base, bytestring
, containers, directory, fetchgit, filepath, generic-lens, ghc
, Glob, hspec-discover, hspec-expectations, lens, lib, mtl
, optparse-applicative, parallel, process, regex-tdfa, tasty
, tasty-golden, tasty-hunit-compat, text, toml-reader, transformers
}:
mkDerivation {
  pname = "weeder";
  version = "2.8.0";
  src = fetchgit {
    url = "https://github.com/fisx/weeder";
    sha256 = "sha256-Cv1H4m5X1iM26svGFdfCVfMO6E/ueaKxCRjrfwsoV7M=";
    rev = "0dae376b4a41d67bdaa4ec55e902df0b3cc58fba"; # https://github.com/ocharles/weeder/pull/165
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    algebraic-graphs async base bytestring containers directory
    filepath generic-lens ghc Glob lens mtl optparse-applicative
    parallel regex-tdfa text toml-reader transformers
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson algebraic-graphs base bytestring containers directory
    filepath ghc hspec-expectations process tasty tasty-golden
    tasty-hunit-compat text toml-reader
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/ocharles/weeder#readme";
  description = "Detect dead code";
  license = lib.licenses.bsd3;
  mainProgram = "weeder";
}
