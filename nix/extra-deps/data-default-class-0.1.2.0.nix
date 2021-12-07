{ mkDerivation, base, lib }:
mkDerivation {
  pname = "data-default-class";
  version = "0.1.2.0";
  sha256 = "4f01b423f000c3e069aaf52a348564a6536797f31498bb85c3db4bd2d0973e56";
  libraryHaskellDepends = [ base ];
  description = "A class for types with a default value";
  license = lib.licenses.bsd3;
}
