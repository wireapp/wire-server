{ mkDerivation, base, bytestring, containers, foldl, hashable, lib
, primitive, QuickCheck, quickcheck-instances, rerebase, tasty
, tasty-hunit, tasty-quickcheck, transformers, unordered-containers
, vector
}:
mkDerivation {
  pname = "deferred-folds";
  version = "0.9.10.1";
  sha256 = "1bea510e31d1d5a8ba2068db468922df3ac0f13966aa3ae70bb1f2130f639c96";
  libraryHaskellDepends = [
    base bytestring containers foldl hashable primitive transformers
    unordered-containers vector
  ];
  testHaskellDepends = [
    QuickCheck quickcheck-instances rerebase tasty tasty-hunit
    tasty-quickcheck
  ];
  homepage = "https://github.com/metrix-ai/deferred-folds";
  description = "Abstractions over deferred folds";
  license = lib.licenses.mit;
}
