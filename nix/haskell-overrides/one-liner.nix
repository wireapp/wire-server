{ mkDerivation, base, bifunctors, contravariant, ghc-prim, HUnit
, lib, profunctors, tagged, transformers
}:
mkDerivation {
  pname = "one-liner";
  version = "1.0";
  sha256 = "c7f4fbea856adcaa145eb4ff9c81bb730f0a1796b24f4075c0a8028ae87a31b6";
  revision = "2";
  editedCabalFile = "0qzjwwsa2bwwplrizzdhwqkxv0l7dfwvhl12rijvyaki2kxc0inw";
  libraryHaskellDepends = [
    base bifunctors contravariant ghc-prim profunctors tagged
    transformers
  ];
  testHaskellDepends = [ base contravariant HUnit ];
  homepage = "https://github.com/sjoerdvisscher/one-liner";
  description = "Constraint-based generics";
  license = lib.licenses.bsd3;
}
