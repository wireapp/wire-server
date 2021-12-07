{ mkDerivation, base, criterion, deepseq, HUnit, lib, parsec
, template-haskell, test-framework, test-framework-hunit
, test-framework-quickcheck2
}:
mkDerivation {
  pname = "network-uri";
  version = "2.6.3.0";
  sha256 = "a01c1389f15d2cc2e847914737f706133bb11f0c5f8ee89711a36a25b7afa723";
  revision = "1";
  editedCabalFile = "0gwvyz2s98lp6cx1jhg8z08n3mv3ycffxm0yvz4whc85s2ppswj3";
  libraryHaskellDepends = [ base deepseq parsec template-haskell ];
  testHaskellDepends = [
    base criterion deepseq HUnit test-framework test-framework-hunit
    test-framework-quickcheck2
  ];
  homepage = "https://github.com/haskell/network-uri";
  description = "URI manipulation";
  license = lib.licenses.bsd3;
}
