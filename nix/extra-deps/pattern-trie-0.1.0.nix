{ mkDerivation, base, bytestring, containers, criterion, deepseq
, doctest, hashable, lib, mtl, QuickCheck, tasty, tasty-quickcheck
, text, unordered-containers
}:
mkDerivation {
  pname = "pattern-trie";
  version = "0.1.0";
  sha256 = "8fd6f6392855d0cc51e2bbb5bdaccc91018e851b5c3b9a897bd6671dd00abed1";
  revision = "1";
  editedCabalFile = "1v9f28gpns5v646hdzn7xfimq2v0sx3rws56r7lfh1qgcfdavy9f";
  libraryHaskellDepends = [
    base bytestring containers deepseq hashable text
    unordered-containers
  ];
  testHaskellDepends = [
    base bytestring containers doctest mtl QuickCheck tasty
    tasty-quickcheck unordered-containers
  ];
  benchmarkHaskellDepends = [
    base bytestring containers criterion deepseq hashable text
  ];
  description = "Pattern tries";
  license = lib.licenses.mpl20;
}
