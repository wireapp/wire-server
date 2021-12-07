{ mkDerivation, attoparsec, base, base-compat
, base-compat-batteries, base-orphans, base16-bytestring
, bytestring, containers, deepseq, Diff, directory, dlist, filepath
, generic-deriving, ghc-prim, hashable, hashable-time
, integer-logarithms, lib, primitive, QuickCheck
, quickcheck-instances, scientific, tagged, tasty, tasty-golden
, tasty-hunit, tasty-quickcheck, template-haskell, text
, th-abstraction, time, time-compat, unordered-containers
, uuid-types, vector
}:
mkDerivation {
  pname = "aeson";
  version = "1.4.7.1";
  sha256 = "07e746655fd9bec81c59927c5617877ff4fcd81d0df45c5fb8ef154fb8f40294";
  revision = "2";
  editedCabalFile = "1y1akkdcn5xrspplzgjyj8yz4b4rl1v29dqsx147fnif0y5sk6b1";
  libraryHaskellDepends = [
    attoparsec base base-compat-batteries bytestring containers deepseq
    dlist ghc-prim hashable primitive scientific tagged
    template-haskell text th-abstraction time time-compat
    unordered-containers uuid-types vector
  ];
  testHaskellDepends = [
    attoparsec base base-compat base-orphans base16-bytestring
    bytestring containers Diff directory dlist filepath
    generic-deriving ghc-prim hashable hashable-time integer-logarithms
    QuickCheck quickcheck-instances scientific tagged tasty
    tasty-golden tasty-hunit tasty-quickcheck template-haskell text
    time time-compat unordered-containers uuid-types vector
  ];
  homepage = "https://github.com/bos/aeson";
  description = "Fast JSON parsing and encoding";
  license = lib.licenses.bsd3;
}
