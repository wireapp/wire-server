{ mkDerivation, base, bytestring, HUnit, lib, mtl, test-framework
, test-framework-hunit, text
}:
mkDerivation {
  pname = "parsec";
  version = "3.1.14.0";
  sha256 = "c72c92e07ee1d2ec0a423b89fdc11e0863725675f3c2d5621509d9cb84545c8c";
  revision = "4";
  editedCabalFile = "0p65q054iaz2117a5qk1428dic4sb41acclys9k00zna24ks7iq3";
  libraryHaskellDepends = [ base bytestring mtl text ];
  testHaskellDepends = [
    base HUnit mtl test-framework test-framework-hunit
  ];
  homepage = "https://github.com/haskell/parsec";
  description = "Monadic parser combinators";
  license = lib.licenses.bsd3;
}
