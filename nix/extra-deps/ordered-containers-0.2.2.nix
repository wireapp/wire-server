{ mkDerivation, base, containers, lib }:
mkDerivation {
  pname = "ordered-containers";
  version = "0.2.2";
  sha256 = "c75ac7330e70cd5d6ac0062b68033779cf15cd986d4ca20f838e016d466d22c9";
  libraryHaskellDepends = [ base containers ];
  description = "Set- and Map-like types that remember the order elements were inserted";
  license = lib.licenses.bsd3;
}
