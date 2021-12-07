{ mkDerivation, base, containers, deepseq, lib, process, random
, splitmix, template-haskell, transformers
}:
mkDerivation {
  pname = "QuickCheck";
  version = "2.14";
  sha256 = "52be1407fb19f5f3b5aa06d41fac745536d08306adaf6dde46a62255d16c384a";
  revision = "1";
  editedCabalFile = "0i8hh6f8y2jxn9hfchhbp9w5mb65fs6yy12z08wyrxxyhr5nllrz";
  libraryHaskellDepends = [
    base containers deepseq random splitmix template-haskell
    transformers
  ];
  testHaskellDepends = [ base deepseq process ];
  homepage = "https://github.com/nick8325/quickcheck";
  description = "Automatic testing of Haskell programs";
  license = lib.licenses.bsd3;
}
