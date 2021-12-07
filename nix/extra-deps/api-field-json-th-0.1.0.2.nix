{ mkDerivation, aeson, base, HUnit, lens, lib, split
, template-haskell, text
}:
mkDerivation {
  pname = "api-field-json-th";
  version = "0.1.0.2";
  sha256 = "b8d49c3869bc8104539c43d5544ed2271d1e68a963440d781ee71d2252b0f724";
  libraryHaskellDepends = [
    aeson base lens split template-haskell text
  ];
  testHaskellDepends = [ aeson base HUnit lens ];
  homepage = "https://github.com/nakaji-dayo/api-field-json-th";
  description = "option of aeson's deriveJSON";
  license = lib.licenses.bsd3;
}
