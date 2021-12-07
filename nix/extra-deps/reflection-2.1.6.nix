{ mkDerivation, base, containers, hspec, hspec-discover, lib
, QuickCheck, template-haskell
}:
mkDerivation {
  pname = "reflection";
  version = "2.1.6";
  sha256 = "bf3e14917ebb329a53701a3cce0afe670f20037a0148dbfa5cbfa574ed6ba6cd";
  revision = "1";
  editedCabalFile = "1bnpkfmagii4mc8258yjy4f4lykflmljkqcifnxpfqv99bszw6pl";
  libraryHaskellDepends = [ base template-haskell ];
  testHaskellDepends = [ base containers hspec QuickCheck ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://github.com/ekmett/reflection";
  description = "Reifies arbitrary terms into types that can be reflected back into terms";
  license = lib.licenses.bsd3;
}
