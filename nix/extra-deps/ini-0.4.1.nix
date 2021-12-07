{ mkDerivation, attoparsec, base, hspec, lib, text
, unordered-containers
}:
mkDerivation {
  pname = "ini";
  version = "0.4.1";
  sha256 = "14293c2a209f938cc3e779132f3411c330636a91b1a58549a154c025518c7c57";
  libraryHaskellDepends = [
    attoparsec base text unordered-containers
  ];
  testHaskellDepends = [ base hspec unordered-containers ];
  homepage = "http://github.com/chrisdone/ini";
  description = "Quick and easy configuration files in the INI format";
  license = lib.licenses.bsd3;
}
