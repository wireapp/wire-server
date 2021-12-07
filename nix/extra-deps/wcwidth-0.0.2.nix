{ mkDerivation, base, containers, lib }:
mkDerivation {
  pname = "wcwidth";
  version = "0.0.2";
  sha256 = "ffc68736a3bbde3e8157710f29f4a99c0ca593c41194579c54a92c62f6c12ed8";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers ];
  homepage = "http://github.com/solidsnack/wcwidth/";
  description = "Native wcwidth";
  license = lib.licenses.bsd3;
}
