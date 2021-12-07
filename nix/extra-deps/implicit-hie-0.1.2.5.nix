{ mkDerivation, attoparsec, base, directory, filepath, filepattern
, hspec, hspec-attoparsec, lib, text, transformers, yaml
}:
mkDerivation {
  pname = "implicit-hie";
  version = "0.1.2.5";
  sha256 = "0ede4a0681f116efe5a34a2a90b09c5cc195ec802b8d4d0304823f34dad2f649";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base directory filepath filepattern text transformers
    yaml
  ];
  executableHaskellDepends = [
    attoparsec base directory filepath filepattern text transformers
    yaml
  ];
  testHaskellDepends = [
    attoparsec base directory filepath filepattern hspec
    hspec-attoparsec text transformers yaml
  ];
  homepage = "https://github.com/Avi-D-coder/implicit-hie#readme";
  description = "Auto generate hie-bios cradles & hie.yaml";
  license = lib.licenses.bsd3;
}
