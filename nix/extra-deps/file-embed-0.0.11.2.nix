{ mkDerivation, base, bytestring, directory, filepath, lib
, template-haskell
}:
mkDerivation {
  pname = "file-embed";
  version = "0.0.11.2";
  sha256 = "a4ca38940a702a5685a2433deb260d3d713dc69ef9c3ac69c9958df08dd3fe2d";
  libraryHaskellDepends = [
    base bytestring directory filepath template-haskell
  ];
  testHaskellDepends = [ base filepath ];
  homepage = "https://github.com/snoyberg/file-embed";
  description = "Use Template Haskell to embed file contents directly";
  license = lib.licenses.bsd3;
}
