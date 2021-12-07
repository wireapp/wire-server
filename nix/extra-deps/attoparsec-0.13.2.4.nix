{ mkDerivation, array, base, bytestring, case-insensitive
, containers, criterion, deepseq, directory, filepath, ghc-prim
, http-types, lib, parsec, QuickCheck, quickcheck-unicode
, scientific, tasty, tasty-quickcheck, text, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "attoparsec";
  version = "0.13.2.4";
  sha256 = "ba66cd6de1749ec92568db1b9c905b43a849f0ad918d45d7b594407a02ebefb2";
  revision = "1";
  editedCabalFile = "0jlipzz2b1jb8yw22rvnhvbnadzcdf3wkwn4svl3j4m6858s0har";
  libraryHaskellDepends = [
    array base bytestring containers deepseq scientific text
    transformers
  ];
  testHaskellDepends = [
    array base bytestring deepseq QuickCheck quickcheck-unicode
    scientific tasty tasty-quickcheck text transformers vector
  ];
  benchmarkHaskellDepends = [
    array base bytestring case-insensitive containers criterion deepseq
    directory filepath ghc-prim http-types parsec scientific text
    transformers unordered-containers vector
  ];
  homepage = "https://github.com/bos/attoparsec";
  description = "Fast combinator parsing for bytestrings and text";
  license = lib.licenses.bsd3;
}
