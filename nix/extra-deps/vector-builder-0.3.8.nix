{ mkDerivation, attoparsec, base, base-prelude, lib, QuickCheck
, quickcheck-instances, rerebase, semigroups, tasty, tasty-hunit
, tasty-quickcheck, vector
}:
mkDerivation {
  pname = "vector-builder";
  version = "0.3.8";
  sha256 = "a39afd7ac50c42de77660d235017be38ef50f792b6a98212accf687445a18073";
  libraryHaskellDepends = [ base base-prelude semigroups vector ];
  testHaskellDepends = [
    attoparsec QuickCheck quickcheck-instances rerebase tasty
    tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/nikita-volkov/vector-builder";
  description = "Vector builder";
  license = lib.licenses.mit;
}
