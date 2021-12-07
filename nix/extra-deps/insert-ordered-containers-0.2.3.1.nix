{ mkDerivation, aeson, base, base-compat, hashable, lens, lib
, optics-core, optics-extra, QuickCheck, semigroupoids, semigroups
, tasty, tasty-quickcheck, text, transformers, unordered-containers
}:
mkDerivation {
  pname = "insert-ordered-containers";
  version = "0.2.3.1";
  sha256 = "ea87f204f7baa6c840e58032169c0e759c4a040955e093669ab2578084290a08";
  revision = "1";
  editedCabalFile = "1s90flzj3039s50r6hx7mqihf8lvarcqb6zps7m12x543gahfcq0";
  libraryHaskellDepends = [
    aeson base base-compat hashable lens optics-core optics-extra
    semigroupoids semigroups text transformers unordered-containers
  ];
  testHaskellDepends = [
    aeson base base-compat hashable lens QuickCheck semigroupoids
    semigroups tasty tasty-quickcheck text transformers
    unordered-containers
  ];
  homepage = "https://github.com/phadej/insert-ordered-containers#readme";
  description = "Associative containers retaining insertion order for traversals";
  license = lib.licenses.bsd3;
}
