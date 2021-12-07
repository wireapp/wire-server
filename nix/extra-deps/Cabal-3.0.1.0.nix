{ mkDerivation, array, base, base-compat, base-orphans, binary
, bytestring, containers, deepseq, Diff, directory, filepath
, integer-logarithms, lib, mtl, optparse-applicative, parsec
, pretty, process, QuickCheck, stm, tagged, tar, tasty
, tasty-golden, tasty-hunit, tasty-quickcheck, temporary, text
, time, transformers, tree-diff, unix
}:
mkDerivation {
  pname = "Cabal";
  version = "3.0.1.0";
  sha256 = "e636eac61cbe9f2ad2c2a32d4710b2abf2d5e54b86e56945bd2835f3dda37147";
  revision = "1";
  editedCabalFile = "060f5gvx00fqr6dggkx1hw7c3p97gd49b7pgvjqrfm6lwmlnxh3z";
  setupHaskellDepends = [ mtl parsec ];
  libraryHaskellDepends = [
    array base binary bytestring containers deepseq directory filepath
    mtl parsec pretty process text time transformers unix
  ];
  testHaskellDepends = [
    array base base-compat base-orphans binary bytestring containers
    deepseq Diff directory filepath integer-logarithms
    optparse-applicative pretty process QuickCheck stm tagged tar tasty
    tasty-golden tasty-hunit tasty-quickcheck temporary text tree-diff
  ];
  doCheck = false;
  homepage = "http://www.haskell.org/cabal/";
  description = "A framework for packaging Haskell software";
  license = lib.licenses.bsd3;
}
