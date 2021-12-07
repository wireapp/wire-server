{ mkDerivation, base, lib, mtl, random }:
mkDerivation {
  pname = "operational";
  version = "0.2.3.5";
  sha256 = "91d479063ae7ed3d0a6ae911bdee550fbf31cf341910f9778046b484c55b4af4";
  revision = "1";
  editedCabalFile = "0sqa37cf5r1ba1fr9gsw68yc0yysgadbxixnni4lhdm4pmni84qv";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base mtl ];
  executableHaskellDepends = [ base mtl random ];
  homepage = "http://wiki.haskell.org/Operational";
  description = "Implementation of difficult monads made easy with operational semantics";
  license = lib.licenses.bsd3;
}
