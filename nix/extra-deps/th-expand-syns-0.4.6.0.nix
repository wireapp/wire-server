{ mkDerivation, base, containers, lib, syb, template-haskell }:
mkDerivation {
  pname = "th-expand-syns";
  version = "0.4.6.0";
  sha256 = "b26b5556091d2bc840d4dfa98768d2dc271f84ea78573bc9d1519be5095de8b7";
  libraryHaskellDepends = [ base containers syb template-haskell ];
  testHaskellDepends = [ base template-haskell ];
  homepage = "https://github.com/DanielSchuessler/th-expand-syns";
  description = "Expands type synonyms in Template Haskell ASTs";
  license = lib.licenses.bsd3;
}
