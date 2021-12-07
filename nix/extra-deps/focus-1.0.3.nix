{ mkDerivation, base, lib, QuickCheck, quickcheck-instances
, rerebase, tasty, tasty-hunit, tasty-quickcheck, transformers
}:
mkDerivation {
  pname = "focus";
  version = "1.0.3";
  sha256 = "353050e457bfe945ce8bda3513206a9d3fd2ac93112791953d2afd32017e060e";
  libraryHaskellDepends = [ base transformers ];
  testHaskellDepends = [
    QuickCheck quickcheck-instances rerebase tasty tasty-hunit
    tasty-quickcheck
  ];
  homepage = "https://github.com/nikita-volkov/focus";
  description = "A general abstraction for manipulating elements of container data structures";
  license = lib.licenses.mit;
}
