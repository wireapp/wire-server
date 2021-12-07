{ mkDerivation, base, lib }:
mkDerivation {
  pname = "haskell-lexer";
  version = "1.1";
  sha256 = "313a15cc643322c8badd148867ce25ca1ffc191df9e7eeec5b10bc08c4b563d5";
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/yav/haskell-lexer";
  description = "A fully compliant Haskell 98 lexer";
  license = lib.licenses.bsd3;
}
