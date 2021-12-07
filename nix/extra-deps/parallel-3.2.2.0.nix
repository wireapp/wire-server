{ mkDerivation, array, base, containers, deepseq, ghc-prim, lib }:
mkDerivation {
  pname = "parallel";
  version = "3.2.2.0";
  sha256 = "170453a71a2a8b31cca63125533f7771d7debeb639700bdabdd779c34d8a6ef6";
  revision = "4";
  editedCabalFile = "02y9cnk5vkz45zkhlimfa3rx6xsx6zy80aryc3q7rvi678gn7zqr";
  libraryHaskellDepends = [ array base containers deepseq ghc-prim ];
  description = "Parallel programming library";
  license = lib.licenses.bsd3;
}
