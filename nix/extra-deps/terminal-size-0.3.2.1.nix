{ mkDerivation, base, lib }:
mkDerivation {
  pname = "terminal-size";
  version = "0.3.2.1";
  sha256 = "b5c23e964756bc13914649a67d63233f59ad0a813abe7cadeb2fc9d586dc9658";
  libraryHaskellDepends = [ base ];
  description = "Get terminal window height and width";
  license = lib.licenses.bsd3;
}
