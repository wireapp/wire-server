{ mkDerivation, base, lib, mtl, QuickCheck, stm, template-haskell
, test-framework, test-framework-hunit, test-framework-quickcheck2
, transformers
}:
mkDerivation {
  pname = "exceptions";
  version = "0.10.4";
  sha256 = "4d0bfb4355cffcd67d300811df9d5fe44ea3594ed63750795bfc1f797abd84cf";
  revision = "2";
  editedCabalFile = "1154g0dqil2xf4wc1v6gndzhnbf5saf2dzf77c6lcjxssx360m6j";
  libraryHaskellDepends = [
    base mtl stm template-haskell transformers
  ];
  testHaskellDepends = [
    base mtl QuickCheck stm template-haskell test-framework
    test-framework-hunit test-framework-quickcheck2 transformers
  ];
  homepage = "http://github.com/ekmett/exceptions/";
  description = "Extensible optionally-pure exceptions";
  license = lib.licenses.bsd3;
}
